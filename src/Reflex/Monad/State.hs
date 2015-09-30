{-# LANGUAGE UndecidableInstances #-}


module Reflex.Monad.State
  ( StateT 
  , State
  
  , runStateT
  , evalSupplyT

  , runSupply
  , evalSupply
  , runSplit
     
 
  ) where


import Reflex
import Reflex.Monad.Class 

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Class

import Control.Monad.RSS.Strict

import Data.Traversable
import Data.Map.Strict (Map)

import Prelude


class Splitable s i | s -> i where
  
  freshId :: s -> (i, s)
  splitSupply :: s -> (s, s)
  



instance Enum a => Splitable [a] [a] where
  freshId []     = ([], [toEnum 0])
  freshId (x:xs) = (x:xs, succ x:xs)
  
  splitSupply xs = (toEnum 0:xs, xs')
    where xs' = snd (freshId xs)

    
    
newtype SupplyT s m a = SupplyT (StateT s m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadFix)
  
  
deriving instance MonadReader st m => MonadReader st (SupplyT s m)
deriving instance MonadWriter w m => MonadWriter w (SupplyT s m)

deriving instance MonadSample t m => MonadSample t (SupplyT s m)
deriving instance MonadHold t m => MonadHold t (SupplyT s m)

type State s a = SupplyT s Identity a

  
instance MonadState st m => MonadState st (SupplyT s m)  where
  get = lift get
  put = lift . put


getFresh :: (Monad m, Splitable s i) => SupplyT s m i
getFresh = SupplyT $ state freshId


getSplit :: (Monad m, Splitable s i) => SupplyT s m s
getSplit = SupplyT $ state splitSupply



runSplit ::  (Monad m, Splitable s i) =>  SupplyT s m a -> State s (m a)
runSplit m = evalSupplyT m <$> getSplit
  

runSupplyT :: Monad m => SupplyT s m a -> s -> m (a, s)
runSupplyT (SupplyT m) = runStateT m 

evalSupplyT :: Monad m => SupplyT s m a -> s -> m a
evalSupplyT (SupplyT m) = evalStateT m 


runSupply :: State s a -> s -> (a, s)
runSupply m  = runIdentity . runSupplyT m 

evalSupply :: State s a -> s -> a
evalSupply m = runIdentity . evalSupplyT m


  
-- | Helpers for switchMapM implementation
runSupplyMap :: (Ord k, Monad m, Splitable s i) =>  Map k (SupplyT s m a) -> s -> (Map k (m a), s) 
runSupplyMap m =  runSupply (traverse runSplit m) 
  
runSupplyMap' :: (Ord k, Monad m, Splitable s i) =>  Map k (Maybe (SupplyT s m a)) -> s -> (Map k (Maybe (m a)), s) 
runSupplyMap' m =  runSupply (traverse (traverse runSplit) m) 
 
instance (MonadSwitch t m, Splitable s i) => MonadSwitch t (SupplyT s m) where

  switchM (Updated initial e) = do    
    s <- getSplit
    rec
      (a, us) <- lift (split <$> switchM (Updated (runSupplyT initial s) $
          attachWith (flip runSupplyT) r e))
      r <- hold' us
    return a


  switchMapM (UpdatedMap initial e) = do   
    (initial', s) <- runSupplyMap initial <$> getSplit
    
    rec
      let (um, us) = split $ attachWith (flip runSupplyMap') r e
      a <- lift (switchMapM (UpdatedMap initial' um))
      r <- hold s us
      
    return a
    
    