{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, OverloadedLists, DeriveFunctor, TemplateHaskell #-}
module Main where

import Prelude

import Control.Monad 
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable hiding (for_)
import Data.Functor
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Control.Concurrent
import qualified Data.Text as T
import Control.Lens
import Data.Semigroup.Applicative
import Data.Tuple

import Reflex
import Reflex.Host.Class

import Reflex.Host.App
import Reflex.Host.App.IO

import System.Exit


data Diff a = Added a | Removed deriving (Show, Functor)

$(makePrisms ''Diff)
  

  
-- schedulePostBuild ::  (MonadAppHost t m) => HostFrame t () -> m ()
-- schedulePostBuild action = performPostBuild_ $ action >> pure mempty
-- 
-- 
-- switchActions :: (MonadAppHost t m, Functor f, Foldable f) => Event t (f (HostFrame t (AppInfo t))) -> m ()
-- switchActions info = do    
--   event <- performEvent $ getApp . foldMap id . fmap Ap <$> info
--   performPostBuild_ $ switchAppInfo mempty event

  
   
   
holdDomList :: (MonadAppHost t r m, Ord k) => Event t (Map k (Diff a)) -> m (Dynamic t (Map k a))
holdDomList changes = foldDyn (flip (ifoldr modify)) mempty changes
  
  where 
    modify k Removed items = Map.delete k items
    modify k (Added item) items = Map.insert k item items
        
diffKeys :: (Ord k) => Map k a -> Map k b -> Map k (Diff b)
diffKeys m m' = (Added <$> m' Map.\\ m)  <> (const Removed <$>  m Map.\\ m')  
     
     
     
diffInput :: (Reflex t, Ord k) => Behavior t (Map k a) -> Event t (Map k b) -> Event t (Map k (Diff b))
diffInput currentItems updatedItems = ffilter (not . Map.null) $ 
  attachWith diffKeys currentItems updatedItems
        

      
domList :: (MonadAppHost t r m, Ord k, Show k) => Event t (Map k (m a)) ->  m (Dynamic t (Map k a))
domList input  = do
  runAppHost <- askRunAppHost
  rec
    let changes = diffInput (current viewsDyn) input

    changedViews <- performEvent $ mapMOf (traverse . _Added) runAppHost <$> changes
    viewsDyn <- holdDomList changedViews 
    
  holdHostF mempty (fmap snd <$> (updated viewsDyn))
  mapDyn (fmap fst) viewsDyn   
  
  
makeView :: (MonadAppHost t r m, Ord k, Show k) =>  Event t (Map k v) -> (k -> Dynamic t v ->  m a) ->  m (Event t (Map k (m a)))
makeView e view = fmap (Map.mapWithKey itemView) <$> e
  where  itemView k v = holdDyn v (fmapMaybe (Map.lookup k) e) >>= view k


  
listWithKey :: (MonadAppHost t r m, Ord k, Show k) => Dynamic t (Map k v) -> (k -> Dynamic t v ->  m a) ->  m (Dynamic t (Map k a))
listWithKey d view = makeView d view >>= domList   
  
  

dummyView :: MonadIOHost t r m => Dynamic t a -> m ()  
dummyView d = do
  
  schedulePostBuild $ do
      sample (current d)
      return ()
      
  performEvent_ $ ffor (updated d) $ \a -> return ()
  performEvent_ $ ffor (updated d) $ \a -> return ()
  performEvent_ $ ffor (updated d) $ \a -> return ()
  performEvent_ $ ffor (updated d) $ \a -> return ()
  

  
main :: IO ()
main = runSpiderHost . hostApp $ test

  


test :: forall t r m. (MonadIOHost t r m) => m ()
test =  do
    
  (addItem, fireAdd) <-  newExternalEvent
  (removeItem, fireRemove) <-  newExternalEvent
  (modifyItem, fireModify) <-  newExternalEvent

  
  
  (items :: Dynamic t (Map Int (Bool, String)))
    <- foldDyn ($) mempty $ mergeWith (.) 
    [ (uncurry Map.insert) <$> addItem 
    , Map.delete <$> removeItem
    , (\(i, str) -> {-at i .-} traverse . _2 .~ str) <$> modifyItem
    ]
    
  listWithKey items $ \k v -> do
    (enabled, str) <- splitDyn v
    dummyView enabled
    dummyView str
  
  rec
    initial <-  getPostBuild   
  
    iter <- performEventAsync $ leftmost [iter, initial] $> do
      forM_ ([1..100]::[Int]) $ \i -> fireAdd (i, (True, "Item " ++ show i))      
      forM_ ([1..100]::[Int]) $ \i -> fireModify (i, "cake")
      forM_ ([1..100]::[Int]) $ \i -> fireRemove i
      
    performEvent_ $ liftIO . print <$> initial
      
      
    counter <- count iter
    performEvent_ $ liftIO . print <$> (updated counter)
    
    performEvent_ $ ffor (ffilter (==1) (updated counter)) $ \_ -> liftIO exitSuccess

  return ()
--     pure mempty
  --     
    
    