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
import Control.Lens hiding (Traversal)

import Data.Tuple
import Control.Applicative

import Reflex
import Reflex.Host.Class

import Reflex.Host.App
import Reflex.Host.App.AppHost
import Criterion.Main


import System.Exit


  

dummyView :: (MonadIOHost t r m, Show a) => Dynamic t a -> m ()  
dummyView d = do
  schedulePostBuild_ $ sample (current d) >> return ()
  performEvent_ $ ffor (updated d) $ \a -> return ()
  


main :: IO ()
main = defaultMain 
  [ benchN 4 50
  , benchN 4 100
  , benchN 16 50
  ]
  
  where
    
    benchN v n = bgroup (show [v, n])
      [ bench "add incremental" $ nfIO (runTest $ testAdd v n)
      , bench ("modify incremental") $ nfIO (runTest $ testModify v n)
      , bench ("remove incremental ") $ nfIO (runTest $ testRemove v n)
      , bench ("bulk add/remove x 20") $ nfIO (runTest $ testBulk v n 20) 
      ]

  
   
  
runTest :: AppHost Spider (HostActions Spider) () -> IO ()
runTest test = runSpiderHost . hostApp $ do
  test >> postQuit
  


  
type Item = (Bool, String)


setup :: (MonadIOHost t r m) => Int -> Map Int Item -> m (Map Int Item -> IO (), Map Int () -> IO ())
setup views initial =  do
    
  (addItem, fireAdd) <-  newExternalEvent
  (removeItem, fireRemove) <-  newExternalEvent

  
  (items :: Dynamic t (Map Int (Bool, String)))
    <- foldDyn ($) initial $ mergeWith (.) 
    [ mappend <$> addItem 
    , flip Map.difference <$> removeItem
    ] 
    
--   performEvent_ $ liftIO . print <$> addItem
    
  listWithKey items $ \k v -> do
    (enabled, str) <- splitDyn v
    
    replicateM views $ do
      dummyView enabled
      dummyView str
    
  return (void . fireAdd, void . fireRemove)
  
  
 
testAdd :: (MonadIOHost t r m) => Int -> Int -> m ()
testAdd v n = do
  (add, remove) <- setup v mempty
  liftIO $ forM_ [1..n] $ \i -> add (item i)
  
  
testModify :: (MonadIOHost t r m) => Int ->  Int -> m ()
testModify v n = do
  (add, remove) <- setup v (makeItems n)
  liftIO $ forM_ [1..n] $ \i ->  add (item i)  
  

testBulk :: (MonadIOHost t r m) => Int -> Int -> Int -> m ()
testBulk v n b = do
  (add, remove) <- setup v mempty
  liftIO $ forM_ [1..n] $ const $ do 
    add $ makeItems b
    remove $ (const () <$> makeItems b)
  
  
makeItems :: Int -> Map Int Item
makeItems n = Map.fromList $ map (\i -> (i, (True, "item " ++ show i))) [1..n]

item :: Int -> Map Int Item
item i = Map.singleton i (True, "item " ++ show i)
  
  
  
testRemove :: (MonadIOHost t r m) => Int ->  Int -> m ()
testRemove v n = do
  (add, remove) <- setup v (makeItems n)
  liftIO $ forM_ [1..n] $ \i ->  remove $ Map.singleton i ()
  
    