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

-- | AppInputs are inputs to the application triggered by the external UI.
--   these are stored in a channel to be processed by the application.
type AppInputs t = HostFrame t [DSum (EventTrigger t)]

type AppInfo t = Event t (HostFrame t ())

-- | This is the environment in which the app host monad runs.
data AppEnv t = AppEnv
    -- | This is the channel to which external events should push their triggers.
    --
    -- Because this is a channel, there is no guarrante that the event that was pushed
    -- is fired directly in the next frame, as there can already be other events waiting
    -- which will be fired first.
  { envEventChan    :: Chan (AppInputs t)
  , envEventFrame   :: IORef [AppInputs t]

  }

-- | An action that is run after a frame. It may return event triggers to fire events.
-- For more information about this type, see the field 'eventsToPerform' of 'AppInfo'.


data AppState t = AppState 
  { _appPostBuild  :: !(HostFrame t ())
  , _appPerform    :: ![Event t (HostFrame t ())]
  }

$(makeLenses ''AppState)  
--------------------------------------------------------------------------------

-- | An implementation of the 'MonadAppHost' typeclass. You should not need to use this
-- type directly. Instead, use the methods provided by the 'MonadAppHost' typeclass and
-- then run your application using 'hostApp' to choose this implementation.
newtype AppHost t a = AppHost
  { unAppHost :: ReaderT (AppEnv t) (StateT (AppState t) (HostFrame t))  a
  }

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
runAppHostFrame :: ReflexHost t => AppEnv t -> AppHost t a -> HostFrame t (a, AppInfo t)
runAppHostFrame env app = do 
  (a, state) <- flip runStateT initial . flip runReaderT env .  unAppHost $ app
  _appPostBuild state
  return $ (a, mergeWith (>>) (_appPerform state))
  
  where initial = AppState (pure ()) []
        
execAppHostFrame :: ReflexHost t => AppEnv t -> AppHost t () -> HostFrame t (AppInfo t)
execAppHostFrame env app = snd <$> runAppHostFrame env app

readFrames :: (ReflexHost t, MonadIO m, MonadReflexHost t m) =>  AppEnv t -> m [DSum (EventTrigger t)]
readFrames env =  do
  performed <- liftIO $ atomicModifyIORef (envEventFrame env) (\a -> ([], a))
  runHostFrame $ concat <$> sequence performed
  
-- | Run an application. The argument is an action in the application host monad,
-- where events can be set up (for example by using 'newExteneralEvent').
--
-- This function will block until the application exits (when one of the 'eventsToQuit'
-- fires).
hostApp :: (ReflexHost t, MonadIO m, MonadReflexHost t m) => AppHost t () -> m ()
hostApp app = loop =<< initHostApp app where
  
  loop (chan, step) = do
    inputs <- liftIO (readChan chan) >>= runHostFrame
    unless (null inputs) $ step inputs >> loop (chan, step)  

-- | Initialize the application using a 'AppHost' monad. This function enables use
-- of use an external control loop. It returns a step function to step the application
-- based on external inputs received through the channel.
-- The step function returns False when one of the 'eventsToQuit' is fired.
initHostApp :: (ReflexHost t, MonadIO m, MonadReflexHost t m)
            => AppHost t () -> m (Chan (AppInputs t), [DSum (EventTrigger t)] -> m ())
initHostApp app = do
  env <- liftIO $ AppEnv <$> newChan <*> newIORef []
  
  toPerform <- runHostFrame $ execAppHostFrame env app
  nextActionEvent <- subscribeEvent toPerform

  let
    go [] = return ()
    go triggers = do
      maybeAction <- fireEventsAndRead triggers $ eventValue nextActionEvent 
      forM_ maybeAction $ \nextAction -> do
        runHostFrame nextAction
        go =<< readFrames env
        
    eventValue :: MonadReadEvent t m => EventHandle t a -> m (Maybe a)
    eventValue = readEvent >=> T.sequenceA

  go =<< readFrames env
  return (envEventChan env, go)
--------------------------------------------------------------------------------

-- | Class providing common functionality for implementing reflex frameworks.
--
-- The host monad is used to setup events from external sources (such as user input) and
-- execute actions in response to events (such as performing some IO). This class contains
-- the primitives required for such a monad, so that higher-level functions can be
-- implemented generically. An implementation is the 'AppHost' monad.
--
-- This Much of the functionality of this class is also provided by its superclasses.
class (ReflexHost t, MonadSample t m, MonadHold t m, MonadReflexCreateTrigger t m,
       MonadIO m, MonadIO (HostFrame t), MonadFix m, MonadFix (HostFrame t))
      => MonadAppHost t m | m -> t where
  -- | Primitive function to create events from external sources.
  --
  -- In reflex, when you create an event (using 'newEventWithTrigger' for example),
  -- you get passed an 'EventTrigger t'. This action returns a function which, given
  -- a trigger and a value for an event, can fire the event. It takes a list of triggers
  -- with values, so you can also use it to fire multiple events in parallel.
  --
  -- Note that the events fired by this function are fired asynchronously. In particular,
  -- if a lot of events are fired, then it can happen that the event queue already
  -- contains other events. In that case, those events will be fired first.
  getPostAsync :: m (AppInputs t -> IO ())
  

  
  getPostFrame :: m (AppInputs t -> IO ())


  -- | Get a function to run the host monad. Useful for implementing dynamic switching.
  --
  -- Running the host monad performs 3 steps:
  --
  -- 1. First, the events and behaviors (using hold) are created. This step does not read
  --    the value of any behavior, since that breaks MonadFix in some cases.
  -- 2. After all events and behaviors have been created, the initial value of behavior
  --    can now be read (using for example 'sample')
  -- 3. This information is then used to create an 'AppInfo' which contains all the
  --    information about the actions to perform in response to the FRP events.
  --
  -- This is why the type of the @run@ function returned by this action is
  -- @m a -> HostFrame t (HostFrame t (AppInfo t), a)@.
  -- Executing outermost @HostFrame t@ will only perform step 1. The inner layer will
  -- then perform step 2, and the returned 'AppInfo' represents step 3.
  getRunAppHost :: m (m a -> HostFrame t (AppInfo t, a))

  -- | Run an action after all other actions have been ran and add information about the
  -- application. After the host monad's actions have been executed, actions registered
  -- with this function will be ran and the returned 'AppInfo's will be merged to get the
  -- final 'AppInfo'.
  --
  -- One use case for this function is to sample the initial values of some behaviors.
  -- This cannot be done directly in the host monad, since that would break MonadFix in
  -- some cases, since it is not lazy enough. Using this function, the sampling can
  -- instead be done after the host monad has finished, so the behavior is not forced too
  -- early.
  performEvent_ :: Event t (HostFrame t ()) -> m ()
  
  schedulePostBuild :: HostFrame t () -> m ()
  

  -- | Directly run a HostFrame action in the host app monad.
  liftHostFrame :: HostFrame t a -> m a

-- | 'AppHost' is an implementation of 'MonadAppHost'.
instance (ReflexHost t, MonadIO (HostFrame t)) => MonadAppHost t (AppHost t) where
  getPostAsync = AppHost $ do
    chan <- envEventChan <$> ask
    return $ liftIO . writeChan chan

  getPostFrame = AppHost $ do
    eventsFrameRef <- envEventFrame <$> ask
    return $ \e -> liftIO $ modifyIORef eventsFrameRef (e:)

  
  getRunAppHost = do
    env <- AppHost ask
    pure $ fmap swap . runAppHostFrame env
    
  performEvent_ event = AppHost $ appPerform %= (event:) 
  schedulePostBuild action = AppHost $ appPostBuild %= (>>action) 
  
  liftHostFrame = AppHost . lift . lift
