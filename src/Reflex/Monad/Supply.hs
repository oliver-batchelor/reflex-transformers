{-# LANGUAGE UndecidableInstances #-}


module Reflex.Monad.Supply
  ( SupplyT 
  , Supply
  , runSupplyT
  , evalSupplyT

  , runSupply
  , evalSupply
  , runSplit
    
  , getFresh
  , getSplit
  
  
  
  ) where


import Reflex
import Reflex.Monad.Class 


import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Reader.Class

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

type Supply s a = SupplyT s Identity a

  
instance MonadState st m => MonadState st (SupplyT s m)  where
  get = lift get
  put = lift . put


getFresh :: (Monad m, Splitable s i) => SupplyT s m i
getFresh = SupplyT $ state freshId


getSplit :: (Monad m, Splitable s i) => SupplyT s m s
getSplit = SupplyT $ state splitSupply



runSplit ::  (Monad m, Splitable s i) =>  SupplyT s m a -> Supply s (m a)
runSplit m = evalSupplyT m <$> getSplit
  

runSupplyT :: Monad m => SupplyT s m a -> s -> m (a, s)
runSupplyT (SupplyT m) = runStateT m 

evalSupplyT :: Monad m => SupplyT s m a -> s -> m a
evalSupplyT (SupplyT m) = evalStateT m 


runSupply :: Supply s a -> s -> (a, s)
runSupply m  = runIdentity . runSupplyT m 

evalSupply :: Supply s a -> s -> a
evalSupply m = runIdentity . evalSupplyT m


tup ::  ((a, b), s) -> (a, (b, s))
tup ((a, b), s)  = (a, (b, s))

unTup :: (a, (b, s)) -> ((a, b), s)
unTup (a, (b, s))  = ((a, b), s)


instance (MonadPerform t m, Splitable s i) => MonadPerform t (SupplyT s m) where
  type Performs (SupplyT s m) a = Performs m a

  collect m = do 
    s <- SupplyT get
    (p, s') <- fmap unTup $ lift  $ 
      collect $ tup <$> runSupplyT m s
      
    SupplyT (put s') 
    return p
        
    
  perform e = do 
    s <- getSplit
    rec
      (a, es) <-  split <$> (lift . perform $ 
        attachWith (flip runSupplyT) r e)
      r <- hold s es 
    
    return a


instance MonadSwitch t m => MonadSwitch t (ReaderT e m) where
  switchM = lift . switchM
  switchMapM = lift . switchMapM

  
--     
    