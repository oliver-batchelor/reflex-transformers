{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Switching where


import Reflex.Class hiding (constant)
import Reflex.Updated

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad
import Control.Monad.Fix
import Control.Applicative
import Data.Semigroup
import Data.List.NonEmpty
import Data.Maybe
import Data.Foldable

import Prelude


class (Reflex t) => Switching t r  where
   -- | Generalization of switchable reactive types (e.g. Event, Behavior)
   switching :: MonadHold t m => r -> Event t r -> m r
   
class (Switching t r, Monoid r) => SwitchMerge t r  where   
   switchMerge :: (MonadFix m, MonadHold t m, Ord k) => Map k r -> Event t (Map k (Maybe r)) -> m r


instance (Switching t a, Switching t b) => Switching t (a, b) where
  switching (a, b) e = liftM2 (,) (switching a $ fst <$> e) (switching b $ snd <$> e)
  
  
instance (SwitchMerge t a, SwitchMerge t b) => SwitchMerge t (a, b) where
  switchMerge initial e = liftA2 (,) (switchMerge' a) (switchMerge' b)
      where (a, b) = split $ UpdatedMap initial e

      

  

-- This will hopefully become a primitive (faster!)
switchMergeEvents ::  (MonadFix m, MonadHold t m, Reflex t, Ord k) =>  UpdatedMap t k (Event t a) -> m (Event t (Map k a))
switchMergeEvents mapChanges = switch . fmap mergeMap  <$> holdMap mapChanges 


instance (Semigroup a, Reflex t) => SwitchMerge t (Event t a) where
  switchMerge initial updates = fmap (foldl1 (<>)) <$> switchMergeEvents (UpdatedMap initial updates)
  
  
instance (Monoid a, Reflex t) => SwitchMerge t (Behavior t a) where
  switchMerge initial updates = pull <$> joinMap <$> holdMap (UpdatedMap initial updates)
    where joinMap m = sample =<< fold <$> sample m
  

mayConcat :: Monoid a => [a] -> Maybe a
mayConcat [] = Nothing
mayConcat xs = Just $ mconcat xs

-- We can optimise [a] a little by eliminating any empty lists before merging
instance (SwitchMerge t a, Monoid a, Reflex t) => SwitchMerge t [a] where
  switchMerge initial updates = pure <$> switchMerge initial' updates'
    where 
      initial' = Map.mapMaybe mayConcat initial
      updates' = fmap (join . fmap mayConcat) <$> updates

  
instance (Switching t a, Monoid a, Reflex t) => Switching t [a]  where
  switching bs updates = pure <$> switching (mconcat bs) (mconcat <$> updates)  
  
  
instance (Reflex t) => SwitchMerge t () where
  switchMerge _ _ = pure ()  


instance (Monoid a, Reflex t) => Switching t (Behavior t a)  where
  switching = switcher
    
instance (Semigroup a, Reflex t) => Switching t (Event t a) where
  switching e updates = switch <$> hold e updates
    
instance (Reflex t) => Switching t () where
  switching _ _ = pure ()
  
switchMerge' :: (Reflex t, SwitchMerge t r, MonadFix m, MonadHold t m, Ord k) => UpdatedMap t k r -> m r 
switchMerge' (UpdatedMap initial e) = switchMerge initial e  

switching' :: (Reflex t, Switching t r, MonadFix m, MonadHold t m) => Updated t r -> m r 
switching' (Updated initial e) = switching initial e  



