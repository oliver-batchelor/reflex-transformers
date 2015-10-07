{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Reflex.Monad.Class
  ( MonadSwitch (..)  
  
  , MonadReflex
  
  , module Reflex
  
  , module Reflex.Switching  
  , module Reflex.Updated
  
  , module Control.Monad.Writer.Class
  
  
  ) where
  

import Reflex
import Reflex.Updated
import Reflex.Switching

import Data.Maybe
import Data.Functor

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

import Control.Monad.Writer.Class

import Prelude


-- Constraint type to capture common usage together
type MonadReflex t m = (Reflex t, MonadHold t m, MonadFix m)    

    
class (MonadReflex t m) => MonadSwitch t m | m -> t where
  
  -- | Map the result of an initial monadic action and updates and swap 
  -- it out with a new one whenever the event provided fires.
  -- returns an 'Updated' giving the initial value plus updates
  -- 
    switchM ::  Updated t (m a) -> m (Updated t a)
    switchM u = do 
      m <- switchMapM (toMap (Just <$> u))
      return $ fromJust <$> fromMap m
    
        
  -- | Similar to holdM but operating on a collection of widgets
  -- provided as an 'UpdatedMap'.
  -- switchM/switchM' can be implemented in terms of switchMapM 
  -- therefore switchMapM is a minimal implementation.
    switchMapM ::  Ord k => UpdatedMap t k (m a) -> m (UpdatedMap t k a)
    
    
  
instance MonadSwitch t m => MonadSwitch t (ReaderT e m) where

  switchM u = do
    env   <- ask
    lift $ switchM (flip runReaderT env <$> u)
    
  
  switchMapM um = do
    env   <- ask
    lift . switchMapM $ flip runReaderT env <$> um

    

instance (MonadSwitch t m, SwitchMerge t w) => MonadSwitch t (WriterT w m) where

  switchM u = do
    (a, w) <- lift $ split <$> switchM (runWriterT <$> u)
    tell =<< switching' w
    return a
    

  switchMapM um = do
    (a, w) <- lift $ split <$> switchMapM (runWriterT <$> um)
    tell =<< switchMerge' w
    return a
    
  

 -- | A few conversions for switchM in terms of switchMapM
maybeToMap :: Maybe a -> Map () a
maybeToMap Nothing  = mempty
maybeToMap (Just a) = Map.singleton () a

mapToMaybe :: Map () a -> Maybe a
mapToMaybe m = listToMaybe $ Map.elems m 
 
toMap :: Reflex t =>  Updated t (Maybe a) -> UpdatedMap t () a
toMap (Updated initial e) = UpdatedMap (maybeToMap initial) (Map.singleton () <$> e)
 
fromMap :: Reflex t => UpdatedMap t () a -> Updated t (Maybe a)
fromMap (UpdatedMap initial e) = Updated (mapToMaybe initial) (fmapMaybe mapToMaybe e)  

