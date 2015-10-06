
-- | Module supporting the implementation of frameworks. You should import this if you
-- want to build your own framework, along with one of the base MonadSwitch classes: 
--
-- Reflex.Monad.App for MonadIO based frameworks
-- or Reflex.Monad.ReflexM for pure frameworks
--

module Reflex.Monad 
  ( module Reflex.Monad.Class
  
  , holdM
  
  , collectMapM
  , collectM
  
  , collect
  
  , Flow (..)
  , flowM
  
  , Chain (..)
  , chainM
  , (>->)
  
  , loopM
  
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens
import Control.Category

import Data.Monoid
import Data.List

import Reflex.Monad.Class
import Reflex.Monad.ReflexM

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import Prelude hiding ((.)) -- Silence AMP warnings


  

holdM :: (MonadSwitch t m) => m a -> Event t (m a) -> m (Dynamic t a)
holdM initial e = holdDyn' =<< switchM (Updated initial e)


withIds :: (MonadReflex t m) => [a] -> Event t [a] -> m (Map Int a, Event t (Map Int a))
withIds initial added = do
  count <- current <$> (foldDyn (+) (genericLength initial)  (genericLength <$> added))
  return (zipFrom 0 initial, attachWith zipFrom count added)
    where
      zipFrom n = Map.fromList . zip [n..] 
    


collect :: (MonadReflex t m) => [(a, Event t ())] -> Event t [(a, Event t ())] -> m (UpdatedMap t Int a)
collect initial added = runReflexM $ collectM (pure <$> initial)  (fmap pure <$> added)
    
    
collectM :: (MonadSwitch t m) => [m (a, Event t ())] -> Event t [m (a, Event t ())] -> m (UpdatedMap t Int a)
collectM initial added = do 
  (initialMap, addedMap) <- withIds initial added
  rec
    
    (values, remove) <- fmap split <$> switchMapM $ UpdatedMap initialMap $ 
        mergeWith (<>) [ fmap Just <$> addedMap, toRemove ]
      
    toRemove <- switchMerge' $ makeRemovals remove
  return values

  where
    makeRemovals = imap (\k -> fmap $ const $ Map.singleton k Nothing)    
  
  
    

collectMapM :: (MonadSwitch t m, Ord k) => Dynamic t (Map k v) -> (k -> Dynamic t v ->  m a) ->  m (Dynamic t (Map k a))
collectMapM input childView =  do
  inputViews <- mapDyn (Map.mapWithKey itemView) input
  let updates = shallowDiff (current inputViews) (updated inputViews)  

  initial <- sample (current inputViews)
  holdMapDyn =<< switchMapM (UpdatedMap initial updates)
  
  where
    itemView k v = holdDyn v (fmapMaybe (Map.lookup k) (updated input)) >>= childView k  
    
    
newtype Flow t m a = Flow { unFlow :: m (a, Event t (Flow t m a)) }

flowM :: (MonadSwitch t m) => Flow t m a -> m (Dynamic t a)
flowM (Flow w) = do
  rec 
    result <- holdM w $ unFlow <$> switch (snd <$> current result)
  mapDyn fst result        
    
  
  
chainM :: (MonadSwitch t m) => Chain t m a b -> a -> m (Event t b)
chainM c a = switchPromptlyDyn <$> flowM (toFlow c a)


loopM :: (MonadSwitch t m) => (a -> m (Event t a)) -> a -> m (Event t a)
loopM f a = do
  rec
    e <- switchPromptlyDyn <$> holdM (f a) (f <$> e)
    
  return e



  
data Chain t m a b where
    Chain :: (a -> m (Event t b)) -> Chain t m a b
    (:>>) ::  (a -> m (Event t b)) -> Chain t m b c ->  Chain t m a c
  

infixr 9 >->
infixr 8 :>>

  
(>->) :: Chain t m a b -> Chain t m b c -> Chain t m a c  
Chain f    >-> c  =  f :>> c 
(f :>> c') >-> c  =  f :>> (c' >-> c) 

toFlow :: (MonadSwitch t m) => Chain t m a b -> a -> Flow t m (Event t b)
toFlow (Chain f) a = Flow $ do 
  e <- f a 
  return (e, end <$ e)
    where end = Flow $ return (never, never)
    
toFlow (f :>> c) a = Flow $ do
  e <- f a
  return (never, toFlow c <$> e)
  
  