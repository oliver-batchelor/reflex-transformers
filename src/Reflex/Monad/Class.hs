{-# LANGUAGE UndecidableInstances #-}

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

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

import Control.Monad.Writer.Class

import Prelude


-- | Constraint type to capture common usage together
type MonadReflex t m = (Reflex t, MonadHold t m, MonadFix m)







class (MonadReflex t m) => MonadSwitch t m | m -> t where

    -- | Map the result of an initial monadic action and updates and swap
    -- it out with a new one whenever the event provided fires.
    -- returns an Updated giving the initial value plus updates

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


type MonadSwitch t m = (Switching t (Writes m), MonadCollect m, MonadPerform t m)

switchM_ :: (Switching t (Writes m), MonadCollect m, MonadPerform t m) => Updated t (m a) -> m (Updated t a)
switchM_ u = do
  (a, w) <- split <$> performU (collect <$> u)
  write =<< switching' w
  return a


switchMapM_ :: (SwitchConcat t (Writes m), Ord k, MonadCollect m, MonadPerform t m) => UpdatedMap t k (m a) -> m (UpdatedMap t k a)
switchMapM_ u = do
  (a, w) <- split <$> performMap (collect <$> u)
  write =<< switchConcat' w
  return a


performU :: (MonadPerform t m) => Updated t (m a) -> m (Updated t a)
performU (Updated m e) = liftA2 Updated m (perform e)

performMap :: (MonadPerform t m, Ord k) => UpdatedMap t k (m a) -> m (UpdatedMap t k a)
performMap (UpdatedMap m e) = liftA2 UpdatedMap (sequence m) $ perform (traverse sequence <$> e)


class Monad m => MonadCollect m  where
  type Writes m

  write   :: Writes m -> m ()
  collect :: m a -> m (a, Writes m)

class (MonadReflex t m) => MonadPerform t m | m -> t where
  perform :: Event t (m a) -> m (Event t a)


instance MonadCollect m => MonadCollect (ReaderT e m) where
  type Writes (ReaderT e m) = Writes m

  write = lift . write
  collect m = ask >>= lift . collect . runReaderT m


instance MonadPerform t m => MonadPerform t (ReaderT e m) where
  perform e = do
    env <- ask
    lift $ perform (flip runReaderT env <$> e)


instance (MonadCollect m, Monoid w) => MonadCollect (WriterT w m) where
  type Writes (WriterT w m) = (w, Writes m)

  write (w, ws) = tell w >> lift (write ws)
  collect m = do
    ((a, w), ws) <- lift (collect (runWriterT m))
    return (a, (w, ws))


instance (MonadPerform t m, Monoid w) => MonadPerform t (WriterT w m) where
  perform e = lift $ perform (fmap fst . runWriterT <$> e)


-- A few conversions for switchM in terms of switchMapM
maybeToMap :: Maybe a -> Map () a
maybeToMap Nothing  = mempty
maybeToMap (Just a) = Map.singleton () a

mapToMaybe :: Map () a -> Maybe a
mapToMaybe m = listToMaybe $ Map.elems m

toMap :: Reflex t =>  Updated t (Maybe a) -> UpdatedMap t () a
toMap (Updated initial e) = UpdatedMap (maybeToMap initial) (Map.singleton () <$> e)

fromMap :: Reflex t => UpdatedMap t () a -> Updated t (Maybe a)
fromMap (UpdatedMap initial e) = Updated (mapToMaybe initial) (fmapMaybe mapToMaybe e)

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
