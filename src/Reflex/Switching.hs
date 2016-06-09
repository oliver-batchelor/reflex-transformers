{-# LANGUAGE UndecidableInstances #-}

module Reflex.Switching
  ( Switching (..)
  , SwitchConcat (..)
  , switching'
  , switchConcat'

  ) where


import Reflex.Class hiding (constant)
import Reflex.Updated

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad
import Control.Monad.Fix
import Control.Applicative
import Data.Semigroup
import Data.Maybe
import Data.Foldable

import Prelude hiding (foldl1)


class (Reflex t) => Switching t r  where
   -- | Generalization of switchable reactive types (e.g. Event, Behavior)
   switching :: MonadHold t m => r -> Event t r -> m r

class (Switching t r, Monoid r) => SwitchConcat t r  where
   -- | Switching for a changing collections of reactive types
   switchConcat :: (MonadFix m, MonadHold t m, Ord k) => Map k r -> Event t (Map k (Maybe r)) -> m r


instance (Switching t a, Switching t b) => Switching t (a, b) where
  switching (a, b) e = liftA2 (,) (switching a $ fst <$> e) (switching b $ snd <$> e)


instance (SwitchConcat t a, SwitchConcat t b) => SwitchConcat t (a, b) where
  switchConcat initial e = liftA2 (,) (switchConcat' a) (switchConcat' b)
      where (a, b) = split $ UpdatedMap initial e



-- This will hopefully become a primitive (faster!)
switchMergeEvents ::  (MonadFix m, MonadHold t m, Reflex t, Ord k) => Map k (Event t a) -> Event t (Map k (Maybe (Event t a))) -> m (Event t (Map k a))
switchMergeEvents initial updates = switchMergeMap initial $ fmap (fromMaybe never) <$> updates

instance (Semigroup a, Reflex t) => SwitchConcat t (Event t a) where
  switchConcat initial updates = fmap (foldl1 (<>)) <$> switchMergeEvents initial updates


instance (Monoid a, Reflex t) => SwitchConcat t (Behavior t a) where
  switchConcat initial updates = pull <$> joinMap <$> holdMap (UpdatedMap initial updates)
    where joinMap m = sample =<< fold <$> sample m


mayConcat :: Monoid a => [a] -> Maybe a
mayConcat [] = Nothing
mayConcat xs = Just $ mconcat xs

-- We can optimise [a] a little by eliminating any empty lists before merging
instance (SwitchConcat t a, Monoid a, Reflex t) => SwitchConcat t [a] where
  switchConcat initial updates = pure <$> switchConcat initial' updates'
    where
      initial' = Map.mapMaybe mayConcat initial
      updates' = fmap (join . fmap mayConcat) <$> updates


instance (Switching t a, Monoid a, Reflex t) => Switching t [a]  where
  switching bs updates = pure <$> switching (mconcat bs) (mconcat <$> updates)


instance (Reflex t) => SwitchConcat t () where
  switchConcat _ _ = pure ()


instance (Monoid a, Reflex t) => Switching t (Behavior t a)  where
  switching = switcher

instance Reflex t => Switching t (Event t a) where
  switching e updates = switch <$> hold e updates

instance (Reflex t) => Switching t () where
  switching _ _ = pure ()


-- | Helper which takes an UpdatedMap as one argument (instead of initial value, update event separately)
switchConcat' :: (Reflex t, SwitchConcat t r, MonadFix m, MonadHold t m, Ord k) => UpdatedMap t k r -> m r
switchConcat' (UpdatedMap initial e) = switchConcat initial e


-- | Helper which takes an Updated as one argument (instead of initial value, update event separately)
switching' :: (Reflex t, Switching t r, MonadFix m, MonadHold t m) => Updated t r -> m r
switching' (Updated initial e) = switching initial e



