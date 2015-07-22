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

-- | Module exposing the internal implementation of the host monad.
-- There is no guarrante about stability of this module.
-- If possible, use 'Reflex.Host.App' instead.
module Reflex.Host.App.Internal where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Dependent.Sum
import Data.Maybe
import Data.Semigroup.Applicative
import Prelude
import Reflex.Class hiding (constant)
import Reflex.Host.Class
import Reflex.Spider

import qualified Data.DList as DL
import qualified Data.Foldable as F
import qualified Data.Traversable as T
--------------------------------------------------------------------------------

-- | AppInputs are inputs to the application triggered by the external UI.
--   these are stored in a channel to be processed by the application.
type AppInputs = [DSum (EventTrigger Spider)]

-- | This is the environment in which the app host monad runs.
data AppEnv = AppEnv
  { -- | This is the channel to which external events should push their triggers.
    --
    -- Because this is a channel, there is no guarrante that the Event Spiderhat was pushed
    -- is fired directly in the next frame, as there can already be other events waiting
    -- which will be fired first.
    envEventChan :: Chan (AppInputs)
  }

-- | An action that is run after a frame. It may return Event Spiderriggers to fire events.
-- For more information about this type, see the field 'eventsToPerform' of 'AppInfo'.
type AppPerformAction = HostFrame Spider (DL.DList (DSum (EventTrigger Spider)))

-- | Information required to set up the application. This also contains all reflex events
-- that the application wants to perform. An 'AppInfo' is called *registered* or *active*
-- if it is currently in use, meaning that the specified events are actually performed.
--
-- The 'AppInfo' represents the output side of a reflex FRP framework. It is used to
-- perform IO actions in response to FRP events, for example. This is called the *effect*
-- of the 'AppInfo'.
data AppInfo  = AppInfo
  { -- | Events that are performed after each frame.
    --
    -- Each event in this list will be checked after a frame. If it is firing with some
    -- 'AppPerformAction', then this action will be executed. The events will be fired
    -- before any other, currently waiting external events contained in the
    -- `envEventChan` are processed.
    --
    -- The event occurrences returned by each 'AppPerformAction' are collected in a list.
    -- If the list is non-empty, then a new frame will be created where all the collected
    -- occurrences are fired at the same time. This process continues until no events
    -- should be fired anymore. Only after this process has finished, external events
    -- contained in the `envEventChan` are processed again.
    --
    -- A common place where you need this is when you want to fire some event in response
    -- to another event, but you need to perform a monadic action such as IO to compute
    -- the value of the response. Using this field, you can perform the monadic action
    -- and then return a trigger to fire the event. This guarrantes that the event is
    -- fired immediately after the frame has finished, even if other, external events
    -- are waiting.
    eventsToPerform :: DL.DList (Event Spider (AppPerformAction))

    -- | Events that, when fired, quit the application.
  , eventsToQuit :: DL.DList (Event Spider ())

    -- | Delayed Event Spiderriggers that will be fired immediately after the initial
    -- application setup has completed, before any external events are processed.
  , triggersToFire :: Ap (HostFrame Spider) (DL.DList (DSum (EventTrigger Spider)))
  }

-- | 'AppInfo' is a monoid. 'mappend' just merges the effects of both app infos.
-- 'mempty' is an 'AppInfo' that has no effect at all when registered.
instance  Monoid AppInfo where
  mempty = AppInfo mempty mempty mempty
  mappend (AppInfo a b c) (AppInfo a' b' c') =
    AppInfo (mappend a a') (mappend b b') (mappend c c')

-- | Produce an 'AppInfo' which only contains 'eventsToPerform'. This is useful in a
-- monoid chain, like @infoToPerform toPerform <> infoToQuit toQuit@.
infoPerform :: DL.DList (Event Spider (AppPerformAction)) -> AppInfo
infoPerform x = mempty { eventsToPerform = x }

-- | Produce an 'AppInfo' which only contains 'eventsToQuit'.
infoQuit ::  DL.DList (Event Spider ()) -> AppInfo
infoQuit x = mempty { eventsToQuit = x }

-- | Produce an 'AppInfo' which only contains 'triggersToFire'.
infoFire ::  HostFrame Spider (DL.DList (DSum (EventTrigger Spider))) -> AppInfo
infoFire x = mempty { triggersToFire = Ap x }

-- | Extract the 'eventsToPerform' and 'eventsToQuit' and merge each into a single event.
appInfoEvents ::  AppInfo -> (Event Spider (AppPerformAction), Event Spider ())
appInfoEvents AppInfo{..} =
  ( mergeWith (liftA2 (<>)) $ DL.toList eventsToPerform
  , leftmost $ DL.toList eventsToQuit
  )

-- | Switch to a different 'AppInfo' whenever an 'Event' fires. Only the events of the
-- currently active application are performed.
--
-- This low-level primitive is used for implementing higher-level functions such as
-- 'switchAppHost', 'performAppHost' or 'dynAppHost'.
switchAppInfo :: AppInfo -> Event Spider (AppInfo) -> HostFrame Spider (AppInfo)
switchAppInfo initialInfo updatedInfo = do
  toPerform <- switch <$> hold initialToPerform updatedToPerform
  toQuit    <- switch <$> hold initialToQuit updatedToQuit
  pure AppInfo
    { eventsToPerform = pure toPerform <> pure (getApp . triggersToFire <$> updatedInfo)
    , eventsToQuit = pure toQuit
    , triggersToFire = triggersToFire initialInfo
    }
 where
  (updatedToPerform, updatedToQuit) = splitE $ fmap appInfoEvents updatedInfo
  (initialToPerform, initialToQuit) = appInfoEvents initialInfo
--------------------------------------------------------------------------------

-- | An implementation of the 'MonadAppHost' typeclass. You should not need to use this
-- type directly. Instead, use the methods provided by the 'MonadAppHost' typeclass and
-- then run your application using 'hostApp' to choose this implementation.
newtype AppHost a = AppHost
  { unAppHost :: ReaderT (AppEnv) (WriterT (Ap (HostFrame Spider) (AppInfo)) (HostFrame Spider)) a
  }
deriving instance  Functor (AppHost)
deriving instance  Applicative (AppHost)
deriving instance  Monad (AppHost)
deriving instance  MonadHold Spider (AppHost)
deriving instance  MonadSample Spider (AppHost)
deriving instance  MonadReflexCreateTrigger Spider (AppHost)
deriving instance MonadIO (AppHost)
deriving instance  MonadFix (AppHost)

-- | Run the application host monad in a reflex host frame and return the produced
-- application info.
execAppHostFrame ::  AppEnv -> AppHost () -> HostFrame Spider (AppInfo)
execAppHostFrame env app = do
  Ap minfo <- execWriterT . flip runReaderT env . unAppHost $ app
  minfo

-- | Run an application. The argument is an action in the application host monad,
-- where events can be set up (for example by using 'newExteneralEvent').
--
-- This function will block until the application exits (when one of the 'eventsToQuit'
-- fires).
hostApp :: AppHost () -> SpiderHost ()
hostApp app = initHostApp app >>= F.mapM_ runStep where
  runStep (chan, step) = do
    nextInput <- liftIO (readChan chan)
    step nextInput >>= flip when (runStep (chan, step))

-- | Initialize the application using a 'AppHost' monad. This function enables use
-- of use an external control loop. It returns a step function to step the application
-- based on external inputs received through the channel.
-- The step function returns False when one of the 'eventsToQuit' is fired.


initHostApp ::  AppHost () -> SpiderHost (Maybe (Chan (AppInputs), AppInputs -> SpiderHost Bool))
initHostApp app = do
  chan <- liftIO newChan
  AppInfo{..} <- runHostFrame $ execAppHostFrame (AppEnv chan) app    
  nextActionEvent <- subscribeEvent $ mergeWith (liftA2 (<>)) $ DL.toList eventsToPerform
  quitEvent <- subscribeEvent $ mergeWith mappend $ DL.toList eventsToQuit

  let
    go [] = return ()
    go triggers = do
      (nextAction, continue) <- lift $ fireEventsAndRead triggers $
        (,) <$> eventValue nextActionEvent <*> fmap isNothing (readEvent quitEvent)
      guard continue
      maybe (return mempty) (lift . runHostFrame) nextAction >>= go . DL.toList

    eventValue :: MonadReadEvent t m => EventHandle t a -> m (Maybe a)
    eventValue = readEvent >=> T.sequenceA

  runMaybeT $ do
    go =<< lift (runHostFrame (DL.toList <$> getApp triggersToFire))
    return (chan, fmap isJust . runMaybeT . go)
--------------------------------------------------------------------------------

-- | Class providing common functionality for implementing reflex frameworks.
--
-- The host monad is used to setup events from external sources (such as user input) and
-- execute actions in response to events (such as performing some IO). This class contains
-- the primitives required for such a monad, so that higher-level functions can be
-- implemented generically. An implementation is the 'AppHost' monad.
--
-- This Much of the functionality of this class is also provided by its superclasses.
-- class (ReflexHost t, MonadSample t m, MonadHold t m, MonadReflexCreateTrigger t m,
--        MonadIO m, MonadIO (HostFrame Spider), MonadFix m, MonadFix (HostFrame Spider))
--       => MonadAppHost m | m -> t where
  -- | Primitive function to create events from external sources.
  --
  -- In reflex, when you create an event (using 'newEventWithTrigger' for example),
  -- you get passed an 'EventTrigger Spider'. This action returns a function which, given
  -- a trigger and a value for an event, can fire the event. It takes a list of triggers
  -- with values, so you can also use it to fire multiple events in parallel.
  --
  -- Note that the events fired by this function are fired asynchronously. In particular,
  -- if a lot of events are fired, then it can happen that the event queue already
  -- contains other events. In that case, those events will be fired first.

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
  -- @m a -> HostFrame Spider (HostFrame Spider (AppInfo), a)@.
  -- Executing outermost @HostFrame Spider@ will only perform step 1. The inner layer will
  -- then perform step 2, and the returned 'AppInfo' represents step 3.

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

  -- | Directly run a HostFrame action in the host app monad.
  

-- | 'AppHost' is an implementation of 'MonadAppHost'.
getAsyncFire :: AppHost ([DSum (EventTrigger Spider)] -> IO ())
getAsyncFire = AppHost $ fmap liftIO . writeChan . envEventChan <$> ask


getRunAppHost :: AppHost (AppHost a -> HostFrame Spider (HostFrame Spider (AppInfo), a))
getRunAppHost = AppHost $ do
  env <- ask
  let rearrange (a, Ap m) = (m, a)
  pure $ fmap rearrange . runWriterT . flip runReaderT env . unAppHost

performPostBuild_ :: HostFrame Spider (AppInfo) -> AppHost ()  
performPostBuild_ mevent = AppHost . tell $ Ap mevent

liftHostFrame :: HostFrame Spider a -> AppHost a
liftHostFrame = AppHost . lift . lift
