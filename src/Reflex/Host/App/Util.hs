module Reflex.Host.App.Util where

import Data.Dependent.Sum

import Reflex.Class hiding (constant)
import Reflex.Host.Class

import Control.Monad
import Control.Monad.IO.Class

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
newEventWithConstructor :: (MonadReflexCreateTrigger t m, MonadIO m, Monoid (f (DSum (EventTrigger t))), Applicative f) 
      => m (Event t a, a -> IO (f (DSum (EventTrigger t))))
newEventWithConstructor = do
  ref <- liftIO $ newIORef Nothing
  event <- newEventWithTrigger (\h -> writeIORef ref Nothing <$ writeIORef ref (Just h))
  return (event, \a -> foldMap pure . fmap (:=> a) <$> liftIO (readIORef ref))