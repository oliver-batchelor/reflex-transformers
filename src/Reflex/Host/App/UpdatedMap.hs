module Reflex.Host.App.UpdatedMap where

import Reflex

import Data.Monoid
import Control.Lens

import Control.Monad
import Control.Monad.Fix

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import Prelude
 

data UpdatedMap t k a = UpdatedMap (Map k a) (Event t (Map k (Maybe a)))   

data Updated t a = Updated a (Event t a)


instance Reflex t => Functor (UpdatedMap t k) where
  fmap f (UpdatedMap initial changes) = UpdatedMap (f <$> initial) (fmap (fmap f) <$> changes)
  
  
instance Reflex t => Functor (Updated t) where
  fmap f (Updated initial changes) = Updated (f initial) (f <$> changes)  
  
  
instance Reflex t => FunctorWithIndex k (UpdatedMap t k) where
  imap f (UpdatedMap initial changes) = UpdatedMap (imap f initial) (imap (\i -> fmap (f i)) <$> changes)


split :: Functor f => f (a, b) -> (f a, f b)
split f = (fst <$> f, snd <$> f)


holdMap :: (Reflex t, MonadHold t m, MonadFix m, Ord k) => UpdatedMap t k a -> m (Dynamic t (Map k a))
holdMap (UpdatedMap initial changes) = foldDyn (flip (ifoldr modify)) initial changes
  
  where 
    modify k Nothing items = Map.delete k items
    modify k (Just item) items = Map.insert k item items

    
hold' :: (Reflex t, MonadHold t m, MonadFix m) => Updated t a -> m (Dynamic t a)    
hold' (Updated initial changes) = holdDyn initial changes
        
        
diffKeys' :: (Ord k) => Map k a -> Map k b -> Map k (Maybe b)
diffKeys' m m' = (Just <$> m' Map.\\ m)  <> (const Nothing <$>  m Map.\\ m')  
          
     
diffKeys :: (Reflex t, Ord k) => Behavior t (Map k a) -> Event t (Map k b) -> Event t (Map k (Maybe b))
diffKeys currentItems updatedItems = ffilter (not . Map.null) $ 
  attachWith diffKeys' currentItems updatedItems
       
 
 
 
