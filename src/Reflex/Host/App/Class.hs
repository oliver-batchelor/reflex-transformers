{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Host.App.Class where

import Data.Dependent.Sum
import Reflex.Class hiding (constant)
import Reflex.Host.Class
import Control.Monad
import Control.Monad.State.Strict

import Prelude


type AppInputs t = HostFrame t [DSum (EventTrigger t)]

  
class (ReflexHost t, MonadSample t m, MonadHold t m, MonadFix m, Monoid (AppResult m))
      => MonadAppHost t m | m -> t where  
        
  type AppResult m  :: *    
  
  hostSwitch  :: AppResult m -> Event t (AppResult m) -> m ()
  hostPerform :: Event t (m a) -> m (Event t (a, AppResult m))
  hostCollect :: m a -> m (a, AppResult m)
  
  
class (ReflexHost t, MonadIO m, MonadIO (HostFrame t), MonadFix (HostFrame t), MonadReflexCreateTrigger t m) 
      => MonadIOHost t m | m -> t

class (MonadIOHost t m) => HasPostFrame t m | m -> t where
  askPostFrame :: m (AppInputs t -> IO ())
  
class (MonadIOHost t m) => HasPostAsync t m | m -> t where
  askPostAsync :: m (AppInputs t -> IO ())
  
class (MonadIOHost t m) => HasPostBuild t m | m -> t where
  schedulePostBuild :: HostFrame t () -> m ()
      
class (MonadIOHost t m) => HasVoidActions t m | m -> t where
  performEvent_ :: Event t (HostFrame t ()) -> m ()
  
-- deriving creates an error requiring ImpredicativeTypes
instance (Reflex t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (StateT s m) where
  newEventWithTrigger initializer = lift $ newEventWithTrigger initializer
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer