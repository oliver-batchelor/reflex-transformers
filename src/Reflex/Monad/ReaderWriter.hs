{-# LANGUAGE UndecidableInstances #-}

module Reflex.Monad.ReaderWriter
  ( ReaderWriterT 
  , ReaderWriter
  
  , runReaderWriterT
  , runReaderWriter

  ) where


import Reflex
import Reflex.Monad.Class 

import Control.Applicative
import Control.Monad
import Control.Monad.Identity

import Control.Monad.Trans.RSS.Strict
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class

import Prelude



    
newtype ReaderWriterT r w m a = ReaderWriterT (RSST r w () m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadFix, MonadReader r, MonadWriter w)
  
  
instance MonadSample t m => MonadSample t (ReaderWriterT r w m) where
  sample = lift . sample
  
instance MonadHold t m => MonadHold t (ReaderWriterT r w m) where
  hold i = lift . hold i



type ReaderWriter r w a = ReaderWriterT r w Identity a

instance  MonadState s m => MonadState s (ReaderWriterT r w m)  where
  get = lift  get
  put = lift . put
  
  
  
runReaderWriterT :: (Monad m, Monoid w) => ReaderWriterT r w m a -> r -> m (a, w)
runReaderWriterT (ReaderWriterT m) r = do 
  (a, _, w) <- runRSST m r ()
  return (a, w)
  
  
runReaderWriter :: (Monoid w) => ReaderWriter r w a -> r -> (a, w) 
runReaderWriter rw r = runIdentity $ runReaderWriterT rw r 


instance (MonadSwitch t m, SwitchMerge t w) => MonadSwitch t (ReaderWriterT r w m) where

  switchM u = do
    env   <- ask
    (a, w) <- lift $ split <$> switchM (flip runReaderWriterT env <$> u)
    tell =<< switching' w
    return a
    
  
  switchMapM um = do
    env   <- ask
    (a, w) <- lift $ split <$> switchMapM (flip runReaderWriterT env <$> um)
    tell =<< switchMerge' w
    return a
    


    
