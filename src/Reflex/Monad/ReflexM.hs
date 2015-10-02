
module Reflex.Monad.ReflexM where


import Control.Applicative
import Control.Monad.Fix
import Reflex.Monad.Class

import Prelude


newtype ReflexM t a = ReflexM { runReflexM :: forall m. MonadReflex t m => m a }

 
instance Functor (ReflexM t) where
  fmap f (ReflexM a) = ReflexM (f <$> a)

  
instance Applicative (ReflexM t) where
  pure a = ReflexM (pure a)
  (<*>) (ReflexM f) (ReflexM a) = ReflexM (f <*> a) 
  
instance Monad (ReflexM t) where
  return a = ReflexM (return a)
  (>>=) (ReflexM m) f = ReflexM (m >>= runReflexM . f) 


instance MonadFix (ReflexM t) where
  mfix f = ReflexM $ mfix (runReflexM . f)

instance MonadSample t (ReflexM t) where
  sample b = ReflexM (sample b)
    
instance MonadHold t (ReflexM t) where
  hold initial e = ReflexM (hold initial e)
  

  
instance Reflex t => MonadPerform t (ReflexM t) where
  type Performs (ReflexM t) a = a

  collect = id
  perform e = return $ pushAlways runReflexM e 
  
  
instance Reflex t => MonadSwitch t (ReflexM t) where
    
    switchM = return
    switchMapM = return
  