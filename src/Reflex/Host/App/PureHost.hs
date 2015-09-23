
module Reflex.Host.App.PureHost where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor

import Data.Maybe
import Reflex.Class hiding (constant)
import Reflex.Host.App.Class

import Prelude


newtype M t a = M { unM :: forall m. MonadReflex t m => m a }

 
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
  { unPureHost ::  StateT r (M t) a
  }

deriving instance Reflex t => Functor (PureHost t r)
deriving instance Reflex t => Applicative (PureHost t r)
deriving instance Reflex t => Monad (PureHost t r)
deriving instance Reflex t => MonadHold t (PureHost t r)
deriving instance Reflex t => MonadSample t (PureHost t r)
deriving instance Reflex t => MonadFix (PureHost t r)  

  
instance (Reflex t, Monoid r) => MonadWriter r (PureHost t r) where
  tell r = PureHost $ modify' (r `mappend`) 

  listen m = do
    (a, r) <- liftPureHost (runPureHost m)
    tell r
    return (a, r)
  
  pass m = do
    ((a, f), r) <- liftPureHost (runPureHost m)
    tell (f r)
    return a  
  
 
liftPureHost :: (Reflex t) => (forall m. (MonadReflex t m) => m a) -> PureHost t r a
liftPureHost ma = PureHost $ lift (M ma)


runPureHost :: (MonadReflex t m, Monoid r) => PureHost t r a -> m (a, r)
runPureHost app =  unM . flip runStateT mempty . unPureHost $ app


execPureHost :: (MonadReflex t m, Monoid r) => PureHost t r a -> m r
execPureHost app = snd <$> runPureHost app
  
  
instance (Reflex t, SwitchMerge t r, Monoid r) => MonadAppHost t r (PureHost t r) where
  
  performHost e = return $ pushAlways runPureHost e
  collect m = liftPureHost (runPureHost m)
  
  
instance (Reflex t, Monoid s, Monoid r) => MapWriter (PureHost t) s r  where  
  mapWriter f ms = do
    (a, (r, b)) <- second f <$> liftPureHost (runPureHost ms)
    tell r
    return (a, b)
    

