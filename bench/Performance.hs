{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, DeriveFunctor, TemplateHaskell #-}
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
import Control.Lens

import Data.Tuple

import Reflex
import Reflex.Host.Class

import Reflex.Host.App
import Reflex.Host.App.AppHost
import Criterion.Main


import System.Exit

  
   
patchMap :: (MonadAppHost t r m, Ord k) => Map k a -> Event t (Map k (Maybe a)) -> m (Dynamic t (Map k a))
patchMap initial changes = foldDyn (flip (ifoldr modify)) initial changes
  
  where 
    modify k Nothing items = Map.delete k items
    modify k (Just item) items = Map.insert k item items



        
diffKeys :: (Ord k) => Map k a -> Map k b -> Map k (Maybe b)
diffKeys m m' = (Just <$> m' Map.\\ m)  <> (const Nothing <$>  m Map.\\ m')  
     
     
     
diffInput :: (Reflex t, Ord k) => Behavior t (Map k a) -> Event t (Map k b) -> Event t (Map k (Maybe b))
diffInput currentItems updatedItems = ffilter (not . Map.null) $ 
  attachWith diffKeys currentItems updatedItems
        

      
domList :: (MonadAppHost t r m, Ord k, Show k) => Dynamic t (Map k (m a)) ->  m (Dynamic t (Map k a))
domList input  = do
  runApp <- askRunApp
  initial <- mapM collectApp =<< sample (current input)
  
  let changes = diffInput (current input) (updated input)

  viewChanges <- performEvent $ mapMOf (traverse . _Just) runApp <$> changes

  holdSwitchMerge (snd <$> initial) (fmap (fmap snd) <$> viewChanges)
  patchMap (fst <$> initial) (fmap (fmap fst) <$> viewChanges)
  
  
  
  

  
listWithKey :: (MonadAppHost t r m, Ord k, Show k) => Dynamic t (Map k v) -> (k -> Dynamic t v ->  m a) ->  m (Dynamic t (Map k a))
listWithKey d view =  do
  views <- mapDyn (Map.mapWithKey itemView) d
  domList views

  where
    itemView k v = holdDyn v (fmapMaybe (Map.lookup k) (updated d)) >>= view k
  

dummyView :: MonadIOHost t r m => Dynamic t a -> m ()  
dummyView d = do
  
  schedulePostBuild $ sample (current d) >> return ()
  schedulePostBuild $ sample (current d) >> return ()
  schedulePostBuild $ sample (current d) >> return ()
  schedulePostBuild $ sample (current d) >> return ()
  
  
  performEvent_ $ ffor (updated d) $ \a -> return ()
  performEvent_ $ ffor (updated d) $ \a -> return ()
  performEvent_ $ ffor (updated d) $ \a -> return ()
  performEvent_ $ ffor (updated d) $ \a -> return ()
  

  
main :: IO ()
main = defaultMain 
  [ bench ("add 1 x " ++ show n) $ nfIO (runTest $ testAdd n)
  , bench ("modify 1 x " ++ show n) $ nfIO (runTest $ testModify n)
  , bench ("add/remove " ++ show n ++ " x " ++ show b) $ nfIO (runTest $ testBulk n b)
  , bench ("remove 1 x " ++ show n) $ nfIO (runTest $ testRemove n)
  ]
  
  where
    n = 100
    b = 20
  
  
runTest :: AppHost Spider (HostActions Spider) () -> IO ()
runTest test = runSpiderHost . hostApp $ do
  test >> postQuit
  
  
type Item = (Bool, String)


setup :: (MonadIOHost t r m) => Map Int Item -> m (Map Int Item -> IO (), Map Int () -> IO ())
setup initial =  do
    
  (addItem, fireAdd) <-  newExternalEvent
  (removeItem, fireRemove) <-  newExternalEvent

  
  (items :: Dynamic t (Map Int (Bool, String)))
    <- foldDyn ($) initial $ mergeWith (.) 
    [ mappend <$> addItem 
    , flip Map.difference <$> removeItem
    ]
    
  listWithKey items $ \k v -> do
    (enabled, str) <- splitDyn v
    dummyView enabled
    dummyView str
    
  return (fireAdd, fireRemove)
  
  
testAdd :: (MonadIOHost t r m) => Int -> m ()
testAdd n = do
  (add, remove) <- setup mempty
  liftIO $ forM_ [1..n] $ \i ->  add $ item i
  
  
testModify :: (MonadIOHost t r m) => Int -> m ()
testModify n = do
  (add, remove) <- setup (makeItems n)
  liftIO $ forM_ [1..n] $ \i ->  add $ item i  
  

testBulk :: (MonadIOHost t r m) => Int -> Int -> m ()
testBulk n b = do
  (add, remove) <- setup mempty
  liftIO $ forM_ [1..n] $ const $ do
    add $ makeItems b
    remove $ (const () <$> makeItems b)
  
  
makeItems :: Int -> Map Int Item
makeItems n = Map.fromList $ map (\i -> (i, (True, "item " ++ show i))) [1..n]

item :: Int -> Map Int Item
item i = Map.singleton i (True, "item " ++ show i)
  
  
  
testRemove :: (MonadIOHost t r m) => Int -> m ()
testRemove n = do
  (add, remove) <- setup (makeItems n)
  forM_ [1..n] $ \i -> liftIO $ remove $ Map.singleton i ()
  postQuit
  
    