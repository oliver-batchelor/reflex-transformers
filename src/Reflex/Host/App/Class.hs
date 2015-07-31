{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TemplateHaskell #-}


module Reflex.Host.App.Class where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens

import Data.Dependent.Sum
import Data.Maybe
import Reflex.Class hiding (constant)
import Reflex.Host.Class
import Data.IORef
import Data.Tuple

import Prelude



import qualified Data.DList as DL
import qualified Data.Foldable as F
import qualified Data.Traversable as T
--------------------------------------------------------------------------------

type AppInputs t = HostFrame t [DSum (EventTrigger t)]

  
class (ReflexHost t, MonadSample t m, MonadHold t m,
       MonadFix m, MonadFix (HostFrame t)), HostSwitch (AppResult m)
      => MonadAppHost t m | m -> t where  
        
  type AppResult m  :: *    
  
  switchAppHost  :: AppResult m -> Event t (AppResult m) -> m ()
  performAppHost :: Event t (m a) -> Event t a
  runAppHost :: m a -> m (AppResult m, a)
  

class HasPostFrame t m | m -> t where
  askPostFrame :: m (AppInputs t -> IO ())
  
class HasPostAsync t m | m -> t where
  askPostAsync :: m (AppInputs t -> IO ())
  
class HasPostBuild t m | m -> t where
  schedulePostBuild :: HostFrame t () -> m ()
      
class HasVoidActions t m | m -> t where
  performEvent_ :: Event t (HostFrame t ()) -> m ()
  


-- deriving creates an error requiring ImpredicativeTypes
instance (Reflex t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (StateT s m) where
  newEventWithTrigger initializer = lift $ newEventWithTrigger initializer
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer