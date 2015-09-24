{-# LANGUAGE UndecidableInstances #-}


module Reflex.Host.App.Supply
  ( SupplyT 
  , runSupplyT
  , evalSupplyT
  , Splitable (..)
  
  , getFresh
  , getSplit
  
  
  
  ) where


import Reflex
import Reflex.Host.App.Class

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Reader.Class


import Prelude


class Splitable s i | s -> i where
  
  fresh :: s -> (i, s)
  split :: s -> (s, s)
  


instance Enum a => Splitable [a] [a] where
  fresh []     = ([], [toEnum 0])
  fresh (x:xs) = (x:xs, succ x:xs)
  
  split xs = (toEnum 0:xs, xs')
    where xs' = snd (fresh xs)

    
    
newtype SupplyT s m a = SupplyT (StateT s m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadFix)
  
  
deriving instance MonadReader st m => MonadReader st (SupplyT s m)
deriving instance MonadWriter w m => MonadWriter w (SupplyT s m)

deriving instance MonadSample t m => MonadSample t (SupplyT s m)
deriving instance MonadHold t m => MonadHold t (SupplyT s m)


  
instance MonadState st m => MonadState st (SupplyT s m)  where
  get = lift get
  put = lift . put


getFresh :: (Monad m, Splitable s i) => SupplyT s m i
getFresh = SupplyT $ state fresh


getSplit :: (Monad m, Splitable s i) => SupplyT s m s
getSplit = SupplyT $ state split

  

runSupplyT :: Monad m => SupplyT s m a -> s -> m (a, s)
runSupplyT (SupplyT m) s = runStateT m s

evalSupplyT :: Monad m => SupplyT s m a -> s -> m a
evalSupplyT (SupplyT m) s = evalStateT m s


  
instance (MonadPerform t r m, Splitable s i) => MonadPerform t r (SupplyT s m) where
  
  perform e = do
    s <- getSplit
    rec
      cur <- hold s (snd . fst <$> r)
      r <- lift $ perform $ attachWith (flip runSupplyT) cur e
    
    return $ rearrange <$> r
      where
        rearrange ((a, _), r) = (a, r)

  collect m = do 
    s   <- SupplyT get
    ((a, s'), r) <- lift $ collect (runSupplyT m s)
    SupplyT (put s')
    return (a, r)
