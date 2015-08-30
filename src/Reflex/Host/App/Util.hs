module Reflex.Host.App.Util where

import Data.Dependent.Sum

import Reflex.Class hiding (constant)
import Reflex.Host.Class
import Reflex

import Data.Monoid
import Control.Lens

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IORef

import Prelude
 

type MapChanges t k a = (Map k a, Event t (Map k (Maybe a)))   

patchMap :: (Reflex t, MonadHold t m, MonadFix m, Ord k) => MapChanges t k a -> m (Dynamic t (Map k a))
patchMap (initial, changes) = foldDyn (flip (ifoldr modify)) initial changes
  
  where 
    modify k Nothing items = Map.delete k items
    modify k (Just item) items = Map.insert k item items

        
diffKeys' :: (Ord k) => Map k a -> Map k b -> Map k (Maybe b)
diffKeys' m m' = (Just <$> m' Map.\\ m)  <> (const Nothing <$>  m Map.\\ m')  
          
     
diffKeys :: (Reflex t, Ord k) => Behavior t (Map k a) -> Event t (Map k b) -> Event t (Map k (Maybe b))
diffKeys currentItems updatedItems = ffilter (not . Map.null) $ 
  attachWith diffKeys' currentItems updatedItems
       
 
 
 
  
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