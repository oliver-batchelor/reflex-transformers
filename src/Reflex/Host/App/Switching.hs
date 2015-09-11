{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Host.App.Switching where


import Reflex.Class hiding (constant)
import Reflex.Dynamic
import Reflex.Host.App.Util

import Data.Map.Strict (Map)

import Control.Monad
import Control.Monad.Fix
import Data.Semigroup
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
  switchMerge initial e = do
    a <- switchMerge (fst <$> initial) (fmap (fmap fst) <$> e)
    b <- switchMerge (snd <$> initial) (fmap (fmap snd) <$> e)
    return (a, b)

    

newtype Behaviors t a = Behaviors { unBehaviors :: [Behavior t a] } deriving (Monoid, Semigroup)
newtype Events t a = Events { unEvents :: [Event t a] } deriving (Monoid, Semigroup)

{-# INLINE events #-}
events :: Event t a -> Events t a
events = Events . pure

{-# INLINE mergeEvents #-}
mergeEvents :: (Reflex t, Semigroup a) => Events t a -> Event t a
mergeEvents = mergeWith (<>) . unEvents

{-# INLINE behaviors #-}
behaviors :: Behavior t a -> Behaviors t a
behaviors = Behaviors . pure

{-# INLINE mergeBehaviors #-}
mergeBehaviors :: (Reflex t, Monoid a) => Behaviors t a -> Behavior t a
mergeBehaviors = mconcat . unBehaviors


-- This will hopefully become a primitive (faster!)
switchMergeEvents ::  (MonadFix m, MonadHold t m, Reflex t, Ord k) =>  UpdatedMap t k (Event t a) -> m (Event t (Map k a))
switchMergeEvents mapChanges = switch . fmap mergeMap . current <$> patchMap mapChanges 

instance (Semigroup a, Reflex t) => SwitchMerge t (Event t a) where
  switchMerge initial updates = fmap (foldl1 (<>)) <$> switchMergeEvents (UpdatedMap initial updates)
  
instance (Semigroup a, Reflex t) => SwitchMerge t (Events t a) where
  switchMerge initial updates = events <$> switchMerge' (mergeEvents <$> UpdatedMap initial updates)

  
instance (Monoid a, Reflex t) => SwitchMerge t (Behavior t a) where
  switchMerge initial updates = pull <$> joinMap . current <$> patchMap (UpdatedMap initial updates)
    where joinMap m = sample =<< fold <$> sample m
  
  
instance (Monoid a, Reflex t) => SwitchMerge t (Behaviors t a) where
  switchMerge initial updates = behaviors <$> switchMerge' (mergeBehaviors <$> UpdatedMap initial updates)  
  
instance (Reflex t) => SwitchMerge t () where
  switchMerge _ _ = pure ()  

instance (Monoid a, Reflex t) => Switching t (Behaviors t a)  where
  switching bs updates = behaviors <$> switching (mergeBehaviors bs) (mergeBehaviors <$> updates)

instance (Semigroup a, Reflex t) => Switching t (Events t a) where
  switching es updates = events <$> switching (mergeEvents es) (mergeEvents <$> updates)
  
    
instance (Monoid a, Reflex t) => Switching t (Behavior t a)  where
  switching = switcher
    
instance (Semigroup a, Reflex t) => Switching t (Event t a) where
  switching e updates = switch <$> hold e updates
    
instance (Reflex t) => Switching t () where
  switching _ _ = pure ()
  
switchMerge' :: (Reflex t, SwitchMerge t r, MonadFix m, MonadHold t m, Ord k) => UpdatedMap t k r -> m r 
switchMerge' (UpdatedMap initial changes) = switchMerge initial changes  



