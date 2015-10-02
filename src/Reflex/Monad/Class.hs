{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Reflex.Monad.Class
  ( MonadSwitch (..)  
  
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
  type Performs m :: *
  
  collect :: m a -> m (a, Performs m)
  perform ::  Event t (m a) -> m (Event t (a, Performs m))
 


class (MonadPerform t m) => MonadSwitch t m | m -> t where
    switchM ::  Updated t (Performs m) -> m ()
    switchMapM ::  Ord k => UpdatedMap t k (Performs m) -> m ()   
    
  
instance MonadPerform t m => MonadPerform t (ReaderT e m) where
  type Performs (ReaderT e m) = Performs m

  collect m = do
    env <- ask
    lift $ collect $ runReaderT m env
  perform e = do
    env <- ask
    lift $ perform (flip runReaderT env <$> e)
  
  

instance MonadSwitch t m => MonadSwitch t (ReaderT e m) where
  switchM = lift . switchM
  switchMapM = lift . switchMapM
  
  
rearrange :: ((a, w), p) -> (a, (w, p))
rearrange ((a, w), p) = (a, (w, p))
    
    
instance (MonadPerform t m, Monoid w) => MonadPerform t (WriterT w m) where
  type Performs (WriterT w m) = (w, Performs m)

  collect m = lift $ rearrange <$> (collect $ runWriterT m)    
  perform e = lift $ fmap rearrange <$> (perform (runWriterT <$> e))

    
instance (MonadSwitch t m, SwitchMerge t w) => MonadSwitch t (WriterT w m) where    
    switchM w = do 
      tell =<< switching' (fst <$> w)
      lift $ switchM (snd <$> w)
      
    switchMapM w = do 
      tell =<< switchMerge' (fst <$> w)
      lift $ switchMapM (snd <$> w)

