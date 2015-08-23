
-- | Module supporting the implementation of frameworks. You should import this if you
-- want to build your own framework.
module Reflex.Host.App 
  ( newExternalEvent, performEvent_, performEvent, performEventAsync
--   , getPostBuild, generateEvent
  
  , schedulePostBuild

  
  , AppInputs, Switching (..)
  , MonadAppWriter (..), MapWriter (..)
  , MonadAppHost(..)
  , MonadIOHost (..)
  
--   , HasPostBuild (..)
  
  , HostActions
  , Events, Behaviors
  
  , holdApp, holdAppF
  , postQuit
  
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
import Reflex.Host.App.Util

import Reflex.Host.App.HostActions

import Reflex.Host.Class

import  Data.Foldable
import  Data.Traversable

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
  
