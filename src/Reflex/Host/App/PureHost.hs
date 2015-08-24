
module Reflex.Host.App.PureHost where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor

import Data.Maybe
import Reflex.Class hiding (constant)
import Reflex.Host.Class


import Reflex.Host.App.Class
import Reflex.Host.App.Switching

import Prelude


newtype M t a = M { unM :: forall m. (MonadHold t m, MonadFix m) => m a }

 
instance Functor (M t) where
  fmap f (M a) = M (f <$> a)

  
instance Applicative (M t) where
  pure a = M (pure a)
  (<*>) (M f) (M a) = M (f <*> a) 
  
instance Monad (M t) where
  return a = M (return a)
  (>>=) (M m) f = M (m >>= unM . f) 


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

  
instance (ReflexHost t, Monoid r) => MonadAppWriter r (PureHost t r) where
  tellApp r = PureHost $ modify (r:) 
  collectApp ma = liftHoldPure (runPureHost ma)
    
 
liftHoldPure :: (Reflex t) => (forall n. (MonadHold t n, MonadFix n) => n a) -> PureHost t r a
liftHoldPure ma = PureHost $ lift (M ma)


runPureHost :: (MonadHold t m, MonadFix m, Monoid r) => PureHost t r a -> m (a, r)
runPureHost app = do 
  (a, r) <- unM . flip runStateT [] . unPureHost $ app
  return (a, mconcat r)

  
instance (ReflexHost t, SwitchMerge t r, Monoid r) => MonadAppHost t r (PureHost t r) where
  
  type Host t (PureHost t r) = M t
  
  performEvent e = return $ push (fmap Just . unM) e
  liftHost (M m) = liftHoldPure m
  askRunApp = return $ \m -> M (runPureHost m)
  
instance (ReflexHost t, Monoid s, Monoid r) => MapWriter (PureHost t) s r  where  
  mapWriter f ms = do
    (a, (r, b)) <- second f <$> liftHoldPure (runPureHost ms)
    tellApp r
    return (a, b)
    

