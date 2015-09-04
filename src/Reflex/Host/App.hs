
-- | Module supporting the implementation of frameworks. You should import this if you
-- want to build your own framework.
module Reflex.Host.App 
  ( newExternalEvent, performEventAsync

  , Switching (..), SwitchMerge (..)
  , MonadWriter (..), MapWriter (..)
  , censor
  
  , MonadAppHost(..)
  , MonadIOHost (..)

  , collect
  , switchAppHost, holdAppHost
  
  , listWithKey
  , collection
  
  
  , HostActions
  , Events, Behaviors
  
  , postQuit
  
  , events, mergeEvents
  , behaviors, mergeBehaviors 
  
  , Workflow (..)
  , workflow
  
  , (>->)
  
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Lens

import Data.Monoid
import Data.List
import Reflex.Class
import Reflex.Dynamic
import Reflex.Host.App.Class
import Reflex.Host.App.Util

import Reflex.Host.App.HostActions

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map



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
  
   

  
collect :: MonadAppHost t r m => m a -> m (a, r)
collect m = do
  runAppHost <- askRunAppHost
  liftHost (runAppHost m)  
 

switchAppHost :: MonadAppHost t r m => Event t (m a) -> m (Event t a)
switchAppHost mChanged = do 
  runAppHost <- askRunAppHost
  updates <- performEvent $ runAppHost <$> mChanged
  tell =<< switching mempty (snd <$> updates) 
  return (fst <$> updates)

-- | Like 'widgetSwitch', but taking the initial postBuild action from another host
-- action.
holdAppHost :: MonadAppHost t r m => m a -> Event t (m a) -> m (Dynamic t a)
holdAppHost mInit mChanged = do
  runAppHost <- askRunAppHost
  (a, r) <- collect mInit
  updates <- performEvent $ runAppHost <$> mChanged
  tell =<< switching r (snd <$> updates) 
  holdDyn a (fst <$> updates)
  
  
        
performList :: (MonadAppHost t r m, Ord k) => UpdatedMap t k (m a) -> m (UpdatedMap t k a)
performList (UpdatedMap initial updates) = do
  runAppHost <- askRunAppHost
  initialViews <- mapM collect initial
  viewUpdates <- performEvent $ mapMOf (traverse . _Just) runAppHost <$> updates
  
  let updatedViews = UpdatedMap initialViews viewUpdates
  
  tell =<< switchMerge' (snd <$> updatedViews)
  return (fst <$> updatedViews)

   
collection :: (MonadAppHost t r m) => [m (a, Event t ())] -> Event t [m (a, Event t ())] -> m (Dynamic t [a])
collection initial added = do
  rec
    count <- current <$> (foldDyn (+) (genericLength initial) $ genericLength <$> added)
    let updates = mergeWith (<>) 
          [ fmap Just <$> attachWith zipFrom count added
          , toRemove 
          ]
  
    updatedViews <- performList (UpdatedMap initialViews updates)
    toRemove <- switchMerge' $ toRemovals $ snd <$> updatedViews
  
  mapDyn Map.elems =<< patchMap (fst <$> updatedViews)

  where
    zipFrom n = Map.fromList . zip [n..] 
    initialViews = zipFrom (0::Integer) initial
    toRemovals = imap (\k -> fmap $ const $ Map.singleton k Nothing)
    
    

listWithKey :: (MonadAppHost t r m, Ord k) => Dynamic t (Map k v) -> (k -> Dynamic t v ->  m a) ->  m (Dynamic t (Map k a))
listWithKey input childView =  do
  inputViews <- mapDyn (Map.mapWithKey itemView) input
  let updates = diffKeys (current inputViews) (updated inputViews)  

  initial <- sample (current inputViews)
  patchMap =<< performList (UpdatedMap initial updates)
  
  where
    itemView k v = holdDyn v (fmapMaybe (Map.lookup k) (updated input)) >>= childView k  
    
    
-- 
newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }

workflow :: MonadAppHost t r m => Workflow t m a -> m (Dynamic t a)
workflow (Workflow w) = do
  rec 
    result <- holdAppHost w $ unWorkflow <$> switch (snd <$> current result)
  mapDyn fst result        
    
  

(>->) :: MonadAppHost t r m => m (Event t b) -> (b -> m (Event t c)) -> m (Event t c)
w >-> f = do
  runAppHost <- askRunAppHost
  (e, r) <- collect w
  next <- performEvent $ runAppHost . f <$> e
  tell =<< switching r (snd <$> next)   
  switchPromptly never (fst <$> next)

      
      
  
  