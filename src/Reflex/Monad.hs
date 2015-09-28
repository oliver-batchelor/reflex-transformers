
-- | Module supporting the implementation of frameworks. You should import this if you
-- want to build your own framework, along with one of the base MonadSwitch classes: 
--
-- Reflex.Monad.App for MonadIO based frameworks
-- or Reflex.Monad.ReflexM for pure frameworks
--

module Reflex.Monad 
  ( module Reflex.Monad.Class
  
  , holdM
  
  , listWithKey
  , collection
  
  , Workflow (..)
  , workflow
  
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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import Prelude hiding ((.)) -- Silence AMP warnings

  

holdM :: (MonadSwitch t m) => m a -> Event t (m a) -> m (Dynamic t a)
holdM initial e = holdDyn' =<< switchM (Updated initial e)


   
collection :: (MonadSwitch t m) => [m (a, Event t ())] -> Event t [m (a, Event t ())] -> m (Dynamic t [a])
collection initial added = do
  rec
    count <- current <$> (foldDyn (+) (genericLength initial) $ genericLength <$> added)
    let updates = mergeWith (<>) 
          [ fmap Just <$> attachWith zipFrom count added
          , toRemove 
          ]
  
    updatedViews <- switchMapM (UpdatedMap initialViews updates)
    toRemove <- switchMerge' $ toRemovals $ snd <$> updatedViews
  
  mapDyn Map.elems =<< holdMapDyn (fst <$> updatedViews)

  where
    zipFrom n = Map.fromList . zip [n..] 
    initialViews = zipFrom (0::Integer) initial
    toRemovals = imap (\k -> fmap $ const $ Map.singleton k Nothing)
    
    

listWithKey :: (MonadSwitch t m, Ord k) => Dynamic t (Map k v) -> (k -> Dynamic t v ->  m a) ->  m (Dynamic t (Map k a))
listWithKey input childView =  do
  inputViews <- mapDyn (Map.mapWithKey itemView) input
  let updates = shallowDiff (current inputViews) (updated inputViews)  

  initial <- sample (current inputViews)
  holdMapDyn =<< switchMapM (UpdatedMap initial updates)
  
  where
    itemView k v = holdDyn v (fmapMaybe (Map.lookup k) (updated input)) >>= childView k  
    
    
-- 
newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }

workflow :: (MonadSwitch t m) => Workflow t m a -> m (Dynamic t a)
workflow (Workflow w) = do
  rec 
    result <- holdM w $ unWorkflow <$> switch (snd <$> current result)
  mapDyn fst result        
    
  
data Chain t m a b where
    Chain :: (a -> m (Event t b)) -> Chain t m a b
    (:>>) ::  (a -> m (Event t b)) -> Chain t m b c ->  Chain t m a c
  

infixr 9 >->
infixr 8 :>>

  
(>->) :: Chain t m a b -> Chain t m b c -> Chain t m a c  
Chain f    >-> c  =  f :>> c 
(f :>> c') >-> c  =  f :>> (c' >-> c) 

toWorkflow :: (MonadSwitch t m) => Chain t m a b -> a -> Workflow t m (Event t b)
toWorkflow (Chain f) a = Workflow $ do 
  e <- f a 
  return (e, end <$ e)
    where end = Workflow $ return (never, never)
    
toWorkflow (f :>> c) a = Workflow $ do
  e <- f a
  return (never, toWorkflow c <$> e)
  
chainM :: (MonadSwitch t m) => Chain t m a b -> a -> m (Event t b)
chainM c a = switchPromptlyDyn <$> workflow (toWorkflow c a)


loopM :: (MonadSwitch t m) => (a -> m (Event t a)) -> a -> m (Event t a)
loopM f a = do
  rec
    e <- switchPromptlyDyn <$> holdM (f a) (f <$> e)
    
  return e


