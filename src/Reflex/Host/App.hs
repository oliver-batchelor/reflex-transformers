{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
-- | Module supporting the implementation of frameworks. You should import this if you
-- want to build your own framework.
module Reflex.Host.App
  ( hostApp
  , newExternalEvent, performEvent_, performEvent
  , getPostBuild, performEventAsync
  , schedulePostBuild, performPostBuild
  , MonadAppHost(..), AppHost(), AppInfo
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Dependent.Sum
import Data.IORef
import Data.Maybe
import Data.Monoid
import Reflex.Class
import Reflex.Dynamic
import Reflex.Host.App.Class
import Reflex.Host.Class

import  Data.Foldable
import  Data.Traversable

import Prelude -- Silence AMP warnings

-- | Create a new event and return a function that can be used to construct an event
-- trigger with an associated value. Note that this by itself will not fire the event.
-- To fire the event, you still need to use either 'performPostBuild_' or 'getAsyncFire'
-- which can fire these event triggers with an associated value.
--
-- Note that in some cases (such as when there are no listeners), the returned function
-- does return 'Nothing' instead of an event trigger. This does not mean that it will
-- neccessarily return Nothing on the next call too though.
newEventWithConstructor
  :: (MonadReflexCreateTrigger t m, MonadIO m) => m (Event t a, a -> IO [DSum (EventTrigger t)])
newEventWithConstructor = do
  ref <- liftIO $ newIORef Nothing
  event <- newEventWithTrigger (\h -> writeIORef ref Nothing <$ writeIORef ref (Just h))
  return (event, \a -> maybeToList . fmap (:=> a) <$> liftIO (readIORef ref))
  
  
  
  
-- | Create a new event from an external event source. The returned function can be used
-- to fire the event.
newExternalEvent :: (MonadReflexCreateTrigger t m, MonadIO m, HasPostAsync t m, MonadIO (HostFrame t)) 
                 => m (Event t a, a -> IO ())
newExternalEvent = do
  fire <- askPostAsync
  (event, construct) <- newEventWithConstructor
  return (event,  liftIO . fire . liftIO . construct)


newFrameEvent :: (MonadReflexCreateTrigger t m, MonadIO m, HasPostFrame t m, MonadIO (HostFrame t)) 
              => m (Event t a,  a -> IO ())
newFrameEvent =  do
  fire <- askPostFrame
  (event, construct) <- newEventWithConstructor
  return (event,  liftIO . fire . liftIO . construct)


-- | Run a monadic action after each frame in which the event fires, and return the result
-- in a new event which is fired immediately following the frame in which the original
-- event fired.
performEvent :: (MonadReflexCreateTrigger t m, MonadIO m, HasPostFrame t m, HasVoidActions t m, MonadIO (HostFrame t)) 
             => Event t (HostFrame t a) -> m  (Event t a)
performEvent event = do
  (result, fire) <- newFrameEvent
  performEvent_ $ (void . liftIO . fire =<<) <$> event
  return result
  
  

-- | Run some IO asynchronously in another thread starting after the frame in which the
-- input event fires and fire an event with the result of the IO action after it
-- completed.
performEventAsync :: (MonadReflexCreateTrigger t m, MonadIO m, HasPostAsync t m, HasVoidActions t m, MonadIO (HostFrame t)) 
                   => Event t (IO a) -> m (Event t a)
performEventAsync event = do
  (result, fire) <- newExternalEvent
  performEvent_ $ liftIO <$> (void . forkIO . void . fire =<<) <$> event
  return result
  
   

-- | Run a HostFrame action after application setup is complete and fire an event with the
-- result.
--
-- Typical use is sampling from Dynamics/Behaviors and providing the result in an Event
-- more convenient to use.
generateEvent ::  (MonadReflexCreateTrigger t m, MonadIO m, HasPostFrame t m, MonadIO (HostFrame t)) 
              => HostFrame t a -> m (Event t a)
generateEvent action = do
  fire <- askPostFrame
  (event, construct) <- newEventWithConstructor
  liftIO . fire $ liftIO . construct =<< action
  return event

-- | Provide an event which is triggered directly after the initial setup of the
-- application is completed.
getPostBuild ::  (MonadReflexCreateTrigger t m, MonadIO m, HasPostFrame t m, MonadIO (HostFrame t)) 
             => m (Event t ())
getPostBuild = generateEvent (return ())


  
performAppHost :: MonadAppHost t m => Event t (m a) -> m (Event t a)
performAppHost mChanged = do 
  updates <- hostPerform mChanged
  hostSwitch mempty (fst <$> updates)
  return (snd <$> updates)
  

-- -- | Like 'switchAppHost', but taking the initial postBuild action from another host
-- -- action.
holdAppHost :: MonadAppHost t m => m a -> Event t (m a) -> m (Dynamic t a)
holdAppHost mInit mChanged = do
  (r, a) <- hostCollect
  updates <- hostPerform mChanged
  hostSwitch r (fst <$> updates)
  holdDyn a (snd <$> updates)
  
