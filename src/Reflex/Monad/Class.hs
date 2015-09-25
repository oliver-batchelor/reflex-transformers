{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Reflex.Host.App.Class
  ( MonadSwitch (..)
  , MonadIOHost (..)
  , IOHost
  
  , MonadReflex
  
  , module Reflex
  
  , module Reflex.Host.App.Switching  
  , module Reflex.Host.App.UpdatedMap
  
  , module Control.Monad.Writer.Class

  , MapWriter (..)
  
  
  ) where


import Data.Dependent.Sum

import Reflex
import Reflex.Class hiding (constant)
import Reflex.Host.Class

import Reflex.Host.App.UpdatedMap
import Reflex.Host.App.Switching



import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

import Control.Monad.Writer.Class

import Prelude


-- Constraint type to capture common usage together
type MonadReflex t m = (Reflex t, MonadHold t m, MonadFix m)    
    
class (MonadReflex t m) => MonadSwitch t m | m -> t where
  
  -- | Map the result of an initial monadic widget and updates and swap 
  -- it out with a new one, whenever the event provided fires.
  -- returns an 'Updated' giving the initial value plus updates
  -- 
    switchM ::  Updated t (m a) -> m (Updated t a)
    
  -- | Like switchM but without an initial action
    switchM' ::  Event t (m a) -> m (Event t a)
    
  -- | Similar to holdM but operating on a collection of widgets
  -- provided as an 'UpdatedMap'.
    switchMapM ::  UpdatedMap t k (m a) -> m (UpdatedMap t k a)
    
    
    
class (MonadWriter r (m r), MonadWriter s (m s)) => MapWriter m s r  where  
  
  -- | Embed one MonadAppWriter in another, a function is used to split the 
  --   result of the inner writer into parts to 'tell' the outer writer
  --   and a part to return.
  mapWriter :: (s -> (r, b)) -> m s a -> m r (a, b) 
  
  
  
instance MonadSwitch t m => MonadSwitch t (ReaderT e m) where

  switchM u = do
    env   <- ask
    lift $ switchM (flip runReaderT env <$> u)
    
  switchM' e = do
    env   <- ask
    lift $ switchM' (flip runReaderT env <$> e)

  
  switchMapM m = do
    env   <- ask
    lift . switchMapM $ flip runReaderT env <$> m

    

instance (MonadSwitch t m, SwitchMerge t w) => MonadSwitch t (WriterT w m) where

  switchM u = do
    (a, w) <- lift (split <$> switchM (runWriterT <$> u))
    tell =<< switching' w
    return a
    
    
  switchM' e = do
    (a, w) <- lift (split <$> switchM' (runWriterT <$> e))
    tell =<< switching mempty w
    return a

  switchMapM m = do
    undefined
  
    
    
type IOHost t m = (ReflexHost t, MonadReflexCreateTrigger t m, MonadIO m, MonadIO (HostFrame t))
    


class (ReflexHost t, MonadSwitch t m, MonadIO m, MonadIO (HostFrame t), 
      MonadReflexCreateTrigger t m) => MonadIOHost t m | m -> t  where
        
  -- | Return a function to post events via IO to a fifo Event queue.
  askPostAsync :: m (HostFrame t [DSum (EventTrigger t)] -> IO ())      

  -- | Run a monadic HostFrame action for the purposes of it's IO effects. 
  -- e.g. for setting properties of an underlying UI or executing an XHR request.
  -- return the result in an Event in the frame immediately following
  performEvent :: Event t (HostFrame t a) -> m (Event t a)
  
  -- | performEvent run just for it's the effects without returning a value
  performEvent_ :: Event t (HostFrame t ()) -> m ()
  
  -- | Run host action for it's effects after construction, 
  -- return the result in an Event.
  schedulePostBuild :: HostFrame t a -> m (Event t a)
  
  -- | Run host action for it's effects after construction.
  schedulePostBuild_ :: HostFrame t () -> m ()  
  

