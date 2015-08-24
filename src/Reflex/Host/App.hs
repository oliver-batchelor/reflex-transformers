
-- | Module supporting the implementation of frameworks. You should import this if you
-- want to build your own framework.
module Reflex.Host.App 
  ( newExternalEvent, performEventAsync

  , Switching (..), SwitchMerge (..)
  , MonadAppWriter (..), MapWriter (..)
  , MonadAppHost(..)
  , MonadIOHost (..)

  , performAppHost, holdAppHost 
  
  
  , HostActions
  , Events, Behaviors
  
  , holdApp, holdSwitchMerge
  , postQuit
  
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans

import Data.Monoid
import Reflex.Class
import Reflex.Dynamic
import Reflex.Host.App.Class
import Reflex.Host.App.Util

import Reflex.Host.App.HostActions
import Reflex.Host.App.Switching

import Data.Map (Map)



import Prelude -- Silence AMP warnings

  
  
-- | Create a new event from an external event source. The returned function can be used
-- to fire the event.
newExternalEvent :: (MonadIOHost t r m) => m (Event t a, a -> IO ())
newExternalEvent = do
  fire <- askPostAsync
  (event, construct) <- newEventWithConstructor
  return (event,  liftIO . fire . liftIO . construct)





postQuit :: (MonadIOHost t r m) => m ()
postQuit = do
  fire <- askPostAsync
  liftIO $ fire (return [])


  
-- | Run some IO asynchronously in another thread starting after the frame in which the
-- input event fires and fire an event with the result of the IO action after it
-- completed.
performEventAsync :: (MonadIOHost t r m) => Event t (IO a) -> m (Event t a)
performEventAsync event = do
  (result, fire) <- newExternalEvent
  performEvent_ $ liftIO <$> (void . forkIO . void . fire =<<) <$> event
  return result
  
   

-- | Run a HostFrame action after application setup is complete and fire an event with the
-- result.
--
-- Typical use is sampling from Dynamics/Behaviors and providing the result in an Event
-- more convenient to use.
-- generateEvent ::  (HasPostBuild t m) => HostFrame t a -> m (Event t a)
-- generateEvent action = do
--   (event, construct) <- newEventWithConstructor
--   generatePostBuild $ liftIO . construct =<< action
--   return event
-- 
-- -- | Provide an event which is triggered directly after the initial setup of the
-- -- application is completed.
-- getPostBuild ::  (HasPostBuild t m) => m (Event t ())
-- getPostBuild = generateEvent (return ())


holdApp :: (MonadHold t m, MonadAppWriter r m, Switching t r) => r -> Event t r -> m ()
holdApp initial updates = tellApp =<< switching initial updates

holdSwitchMerge :: (MonadHold t m, MonadFix m, MonadAppWriter r m, Ord k, SwitchMerge t r) => Map k r -> Event t (Map k (Maybe r)) -> m ()
holdSwitchMerge initial updates = tellApp =<< switchMerge initial updates

 

performAppHost :: MonadAppHost t r m => Event t (m a) -> m (Event t a)
performAppHost mChanged = do 
  runApp <- askRunApp
  updates <- performEvent $ runApp <$> mChanged
  holdApp mempty (snd <$> updates) 
  return (fst <$> updates)

-- -- | Like 'switchAppHost', but taking the initial postBuild action from another host
-- -- action.
holdAppHost :: MonadAppHost t r m => m a -> Event t (m a) -> m (Dynamic t a)
holdAppHost mInit mChanged = do
  runApp <- askRunApp
  (a, r) <- collectApp mInit
  updates <- performEvent $ runApp <$> mChanged
  holdApp r (snd <$> updates) 
  holdDyn a (fst <$> updates)
  
