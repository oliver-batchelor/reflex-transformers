{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Host.App.Switching where

import Data.Dependent.Sum

import Reflex.Class hiding (constant)
import Reflex.Dynamic

import Control.Lens

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad
import Control.Monad.Fix
import Data.Semigroup
import Data.Maybe
import Data.Foldable

import Prelude


class (Reflex t) => Switching t r | r -> t where
   -- | Generalization of switchable reactive types (e.g. Event, Behavior)
   switching :: MonadHold t m => r -> Event t r -> m r
   
class (Switching t r, Monoid r) => SwitchMerge t r | r -> t where   
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


events :: Event t a -> Events t a
events = Events . pure


mergeEvents :: (Reflex t, Semigroup a) => Events t a -> Event t a
mergeEvents = mergeWith (<>) . unEvents

behaviors :: Behavior t a -> Behaviors t a
behaviors = Behaviors . pure

mergeBehaviors :: (Reflex t, Monoid a) => Behaviors t a -> Behavior t a
mergeBehaviors = mconcat . unBehaviors



instance (Monoid a, Reflex t) => Switching t (Behaviors t a)  where
  switching bs updates = behaviors <$> switcher (mergeBehaviors bs) (mergeBehaviors <$> updates)

instance (Semigroup a, Reflex t) => Switching t (Events t a) where
  switching es updates = events <$> switchPromptly (mergeEvents es) (mergeEvents <$> updates)
  
  
patchMap :: (MonadHold t m, MonadFix m, Reflex t, Ord k) => Map k a -> Event t (Map k (Maybe a)) -> m (Dynamic t (Map k a))
patchMap initial changes = foldDyn (flip (ifoldr modify)) initial changes
  
  where 
    modify k Nothing items = Map.delete k items
    modify k (Just item) items = Map.insert k item items  
    
    
-- This will hopefully become a primitive (faster!)
switchMergeEvents ::  (MonadFix m, MonadHold t m, Reflex t, Ord k) =>  Map k (Event t a) -> Event t (Map k (Maybe (Event t a))) -> m (Event t (Map k a))
switchMergeEvents initial updates = switch . fmap (mergeMap) . current <$> patchMap initial updates 

  
instance (Semigroup a, Reflex t) => SwitchMerge t (Events t a) where
  switchMerge initial updates = do 
    e <- switchMergeEvents  (mergeEvents <$> initial) (fmap (fmap mergeEvents) <$> updates)
    return $ events (foldl1 (<>) <$> e)
     
    
    
  
  