{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Reflex.Monad.Class
  ( MonadSwitch (..)  
  , MonadPerform(..)
  
  , collect_
  
  , MonadReflex
  
  , module Reflex
  
  , module Reflex.Switching  
  , module Reflex.Updated
  
  , module Control.Monad.Writer.Class
  
  
  ) where
  

import Reflex
import Reflex.Updated
import Reflex.Switching

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

import Control.Monad.Writer.Class

import Prelude


-- Constraint type to capture common usage together
type MonadReflex t m = (Reflex t, MonadHold t m, MonadFix m)    


class (MonadReflex t m) => MonadPerform t m | m -> t where
  type Performs m a :: * 
  
  collect :: m (a, b) -> m (Performs m a, b)
  perform ::  Event t (m a) -> m (Event t a)
 

collect_ :: MonadPerform t m =>  m a -> m (Performs m a)
collect_ m = fst <$> collect ((, ()) <$> m)

 

class (MonadPerform t m) => MonadSwitch t m | m -> t where
    switchM ::  Updated t (Performs m a) -> m (Updated t a)
    switchMapM ::  Ord k => UpdatedMap t k (Performs m a) -> m (UpdatedMap t k a)   
    
  
instance MonadPerform t m => MonadPerform t (ReaderT e m) where
  type Performs (ReaderT e m) a = Performs m a

  collect m = ask >>= lift . collect . runReaderT m 
  perform e = do
    env <- ask
    lift $ perform (flip runReaderT env <$> e)
  
  

instance MonadSwitch t m => MonadSwitch t (ReaderT e m) where
  switchM = lift . switchM
  switchMapM = lift . switchMapM
  
  
swap2 :: ((a, b), w) -> ((a, w), b)
swap2 ((a, b), w) = ((a, w), b)
    
instance (MonadPerform t m, Monoid w) => MonadPerform t (WriterT w m) where
  type Performs (WriterT w m) a = Performs m (a, w)

  collect = lift . collect . fmap swap2 . runWriterT
  perform e = lift . perform $ fmap fst . runWriterT <$> e

    
instance (MonadSwitch t m, SwitchMerge t w) => MonadSwitch t (WriterT w m) where    
    switchM u = do 
      (a, w) <- lift $ split <$> switchM u
      tell =<< switching' w
      return a
            
    switchMapM um = do
      (a, w) <- lift $ split <$> switchMapM um
      tell =<< switchMerge' w
      return a
    

