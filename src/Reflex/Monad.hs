
-- | Module supporting the implementation of frameworks. You should import this if you
-- want to build your own framework, along with one of the base MonadSwitch classes: 
--
-- Reflex.Monad.App for MonadIO based frameworks
-- or Reflex.Monad.ReflexM for pure frameworks
--

module Reflex.Monad 
  ( module Reflex.Monad.Class
  
  , widgetHold
  
  , mapView
  
  , collection
  , collect
  
  , Workflow (..)
  , workflow
  
  , Chain (..)
  , chain
  , (>->)
  
  , loop
  
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens

import Data.Monoid
import Data.List
import Data.Functor

import Reflex.Monad.Class
import Reflex.Monad.ReflexM

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import Prelude 


  

widgetHold :: (MonadSwitch t m) => m a -> Event t (m a) -> m (Dynamic t a)
widgetHold initial e = holdDyn' =<< switchM (Updated initial e)


withIds :: (MonadReflex t m) => [a] -> Event t [a] -> m (Map Int a, Event t (Map Int a))
withIds initial added = do
  total <- current <$> foldDyn (+) (genericLength initial)  (genericLength <$> added)
  return (zipFrom 0 initial, attachWith zipFrom total added)
    where
      zipFrom n = Map.fromList . zip [n..] 
    


collect :: (MonadReflex t m) => [(a, Event t ())] -> Event t [(a, Event t ())] -> m (UpdatedMap t Int a)
collect initial added = runReflexM $ collection (pure <$> initial)  (fmap pure <$> added)
    
    
collection :: (MonadSwitch t m) => [m (a, Event t ())] -> Event t [m (a, Event t ())] -> m (UpdatedMap t Int a)
collection initial added = do 
  (initialMap, addedMap) <- withIds initial added
  rec
    
    (values, remove) <- fmap split <$> switchMapM $ UpdatedMap initialMap $ 
        mergeWith (<>) [ fmap Just <$> addedMap, toRemove ]
      
    toRemove <- switchMerge' $ makeRemovals remove
  return values

  where
    makeRemovals = imap (\k -> fmap $ const $ Map.singleton k Nothing)    
  
  

    

mapView :: (MonadSwitch t m, Ord k) => Dynamic t (Map k v) -> (k -> Dynamic t v ->  m a) ->  m (Dynamic t (Map k a))
mapView input childView =  do
  inputViews <- mapDyn (Map.mapWithKey itemView) input
  let updates = shallowDiff (current inputViews) (updated inputViews)  

  initial <- sample (current inputViews)
  holdMapDyn =<< switchMapM (UpdatedMap initial updates)
  
  where
    itemView k v = holdDyn v (fmapMaybe (Map.lookup k) (updated input)) >>= childView k  
    
    
    
-- | Provide a widget which swaps itself out for another widget upon an event
-- (recursively)
-- Useful if the sequence of widgets needs to return a value (as opposed to passing it 
-- down the chain).


newtype Workflow t m a = Workflow { unFlow :: m (a, Event t (Workflow t m a)) }

workflow :: (MonadSwitch t m) => Workflow t m a -> m (Dynamic t a)
workflow (Workflow w) = do
  rec 
    result <- widgetHold w $ unFlow <$> switch (snd <$> current result)
  mapDyn fst result        
    
  
  
-- | Provide a way of chaining widgets of type (a -> m (Event t b))
-- where one widgets swaps out the old widget.
-- De-couples the return type as compared to using 'workflow'

chain :: (MonadSwitch t m) => Chain t m a b -> a -> m (Event t b)
chain c a = switchPromptlyDyn <$> workflow (toFlow c a)


-- | Provide a way of looping (a -> m (Event t a)), each iteration switches
-- out the previous iteration.
-- Can be used with 
loop :: (MonadSwitch t m) => (a -> m (Event t a)) -> a -> m (Event t a)
loop f a = do
  rec
    e <- switchPromptlyDyn <$> widgetHold (f a) (f <$> e)
    
  return e



  
data Chain t m a b where
    Chain :: (a -> m (Event t b)) -> Chain t m a b
    (:>>) ::  (a -> m (Event t b)) -> Chain t m b c ->  Chain t m a c
  

infixr 9 >->
infixr 8 :>>

  
(>->) :: Chain t m a b -> Chain t m b c -> Chain t m a c  
Chain f    >-> c  =  f :>> c 
(f :>> c') >-> c  =  f :>> (c' >-> c) 

toFlow :: (MonadSwitch t m) => Chain t m a b -> a -> Workflow t m (Event t b)
toFlow (Chain f) a = Workflow $ do 
  e <- f a 
  return (e, end <$ e)
    where end = Workflow $ return (never, never)
    
toFlow (f :>> c) a = Workflow $ do
  e <- f a
  return (never, toFlow c <$> e)
  
  