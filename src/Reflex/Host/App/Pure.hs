
module Reflex.Host.App.Pure where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens 
import Data.Traversable

import Data.Maybe
import Reflex.Class hiding (constant)
import Reflex.Host.Class
import Reflex.Host.App.Class

import Reflex.Host.App


import Prelude


newtype M t a = M { unM :: forall m. (MonadHold t m, MonadFix m) => m a }

 
instance Functor (M t) where
  fmap f (M a) = M (f <$> a)

  
instance Applicative (M t) where
  pure a = M (pure a)
  
instance Monad (M t) where
  return a = M (return a)

instance MonadFix (M t) where
  mfix f = M $ mfix (unM . f)

instance MonadSample t (M t) where
  sample b = M (sample b)
    
instance MonadHold t (M t) where
  hold initial e = M (hold initial e)
  
  
newtype PureHost t r a = PureHost
  { unPureHost ::  StateT [r] (M t) a
  }

deriving instance ReflexHost t => Functor (PureHost t r)
deriving instance ReflexHost t => Applicative (PureHost t r)
deriving instance ReflexHost t => Monad (PureHost t r)
deriving instance ReflexHost t => MonadHold t (PureHost t r)
deriving instance ReflexHost t => MonadSample t (PureHost t r)
deriving instance ReflexHost t => MonadFix (PureHost t r)  


  
instance (ReflexHost t, Monoid r) => HostWriter r (PureHost t r) where
  tellHost r = PureHost $ modify (r:) 
  collectHost ma = liftHoldPure (runPureHost ma)
    
 
liftHoldPure :: (Reflex t) => (forall n. (MonadHold t n, MonadFix n) => n a) -> PureHost t r a
liftHoldPure ma = PureHost $ lift (M ma)



runPureHost :: (MonadHold t m, MonadFix m, Monoid r) => PureHost t r a -> m (a, r)
runPureHost app = do 
  (a, r) <- unM . flip runStateT [] . unPureHost $ app
  return (a, mconcat r)

  
instance (ReflexHost t, Switchable t r, Monoid r, HasHostActions t r) => MonadAppHost t r (PureHost t r) where

  --performHost :: Event t (m a) -> m (Event t (a, r))
  --liftHold :: (forall m. (MonadHold t m', MonadFix m') => m' a) -> m a
  performHost = 
  
  liftHold = liftHoldPure
  
{-
-- | Run the application host monad in a reflex host frame and return the produced
-- application info.
runPureHostFrame :: (ReflexHost t, Monoid r) => EventChannels t -> PureHost t r a -> HostFrame t (a, r)
runPureHostFrame env app = do 
  (a, actions) <- flip runStateT initial . flip runReaderT env . unPureHost $ app
  _hostPostBuild actions
  return (a, mconcat $ _hostActions actions)
    where initial = HostState (return ()) []
  
execPureHostFrame :: (ReflexHost t, Monoid r) => EventChannels t -> PureHost t r a -> HostFrame t r
execPureHostFrame env app = snd <$> runPureHostFrame env app-}
