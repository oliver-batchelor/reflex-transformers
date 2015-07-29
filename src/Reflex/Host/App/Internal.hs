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

-- | Module exposing the internal implementation of the host monad.
-- There is no guarrante about stability of this module.
-- If possible, use 'Reflex.Host.App' instead.
module Reflex.Host.App.Internal where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Dependent.Sum
import Data.Maybe
import Data.Semigroup.Applicative
import Prelude
import Reflex.Class hiding (constant)
import Reflex.Host.Class
import Data.IORef
import Data.Tuple


import Control.Lens

import qualified Data.DList as DL
import qualified Data.Foldable as F
import qualified Data.Traversable as T
--------------------------------------------------------------------------------


class Monoid out => HostSwitch out where
  hostSwitch :: Event t out -> out -> HostFrame t out

  

newtype AppHost t env out a = AppHost
  { unAppHost :: ReaderT env (StateT out (HostFrame t))  a
  }

-- deriving creates an error requiring ImpredicativeTypes
instance (Reflex t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (StateT s m) where
  newEventWithTrigger initializer = lift $ newEventWithTrigger initializer
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer
  
deriving instance ReflexHost t => Functor (AppHost t)
deriving instance ReflexHost t => Applicative (AppHost t)
deriving instance ReflexHost t => Monad (AppHost t)
deriving instance ReflexHost t => MonadHold t (AppHost t)
deriving instance ReflexHost t => MonadSample t (AppHost t)
deriving instance ReflexHost t => MonadReflexCreateTrigger t (AppHost t)
deriving instance (MonadIO (HostFrame t), ReflexHost t) => MonadIO (AppHost t)
deriving instance ReflexHost t => MonadFix (AppHost t)


-- | Run the application host monad in a reflex host frame and return the produced
-- application info.
runAppHostFrame :: (ReflexHost t, Monoid out) => env -> AppHost t env out a -> HostFrame t (a, out)
runAppHostFrame env app = flip runStateT mempty . flip runReaderT env .  unAppHost $ app

        
execAppHostFrame :: (ReflexHost t, Monoid out)  => env -> AppHost t env out a -> HostFrame t (MergeResult t out)
execAppHostFrame env app = snd <$> runAppHostFrame env app


  

  {-
  
  getPostAsync :: m (AppInputs t -> IO ())
  getPostAsync = AppHost $ do
    chan <- envEventChan <$> ask
    return $ liftIO . writeChan chan

  getPostFrame :: m (AppInputs t -> IO ())
  getPostFrame = AppHost $ do
    eventsFrameRef <- envEventFrame <$> ask
    return $ \e -> liftIO $ modifyIORef eventsFrameRef (e:)

  getRunAppHost :: m (m a -> HostFrame t (AppInfo t, a))
  getRunAppHost = do
    env <- AppHost ask
    pure $ fmap swap . runAppHostFrame env
    
    
  performEvent_ :: Event t (HostFrame t ()) -> m ()
  performEvent_ event = AppHost $ appPerform %= (event:) 
  
  schedulePostBuild :: HostFrame t () -> m ()
  schedulePostBuild action = AppHost $ appPostBuild %= (>>action) 
  
  liftHostFrame :: HostFrame t a -> m a
  liftHostFrame = AppHost . lift . lift-}
