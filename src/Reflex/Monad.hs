
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
  
  , (>->)
  
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens

import Data.Monoid
import Data.List

import Reflex.Monad.Class

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import Prelude -- Silence AMP warnings

  

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
    
  

(>->) :: (MonadSwitch t m) => m (Event t b) -> (b -> m (Event t c)) -> m (Event t c)
w >-> f = do
  undefined
  
--   (e, r) <- collect (w >>= onceE)
--   
--   next <- perform $ f <$> e
--   tell =<< switching r (snd <$> next)   
--   switchPromptly never (fst <$> next)

