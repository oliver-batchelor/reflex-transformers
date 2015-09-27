module Reflex.Monad.IO 
  ( newEventWithConstructor
  , newExternalEvent
  , postQuit
  , performEventAsync
  
  , module Reflex.Monad.IO.Class
  )
  
  where

import Data.Dependent.Sum

import Reflex.Monad.IO.Class
import Reflex.Host.Class

import Data.Monoid

import Control.Monad
import Control.Monad.IO.Class

import Control.Concurrent

import Data.IORef

import Prelude
 
 
 
  
-- | Create a new event and return a function that can be used to construct an event
-- trigger with an associated value. Note that this by itself will not fire the event.
-- To fire the event, you still need to use either 'performPostBuild_' or 'getAsyncFire'
-- which can fire these event triggers with an associated value.
--
-- Note that in some cases (such as when there are no listeners), the returned function
-- does return 'Nothing' instead of an event trigger. This does not mean that it will
-- neccessarily return Nothing on the next call too though.
{-# INLINE newEventWithConstructor #-}
newEventWithConstructor :: (MonadReflexIO t m, Monoid (f (DSum (EventTrigger t))), Applicative f) 
                        => m (Event t a, a -> IO (f (DSum (EventTrigger t))))
newEventWithConstructor = do
  ref <- liftIO $ newIORef Nothing
  event <- newEventWithTrigger (\h -> writeIORef ref Nothing <$ writeIORef ref (Just h))
  return (event, \a -> foldMap pure . fmap (:=> a) <$> liftIO (readIORef ref))
  
  
-- | Create a new event from an external event source. The returned function can be used
-- to fire the event.
newExternalEvent :: (MonadAppHost t m) => m (Event t a, a -> IO ())
newExternalEvent = do
  fire <- askPostAsync
  (event, construct) <- newEventWithConstructor
  return (event,  liftIO . fire . liftIO . construct)



postQuit :: (MonadAppHost t m) => m ()
postQuit = do
  fire <- askPostAsync
  liftIO $ fire (return [])


  
-- | Run some IO asynchronously in another thread starting after the frame in which the
-- input event fires and fire an event with the result of the IO action after it
-- completed.
performEventAsync :: (MonadAppHost t m) => Event t (IO a) -> m (Event t a)
performEventAsync event = do
  (result, fire) <- newExternalEvent
  performEvent_ $ liftIO <$> (void . forkIO . void . fire =<<) <$> event
  return result


  