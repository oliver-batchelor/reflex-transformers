
module Reflex.Host.App.ReflexM where


import Control.Applicative
import Control.Monad.Fix
import Reflex.Host.App.Class

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
  
  


  
instance Reflex t => MonadSwitch t (ReflexM t) where
    
    switchM (Updated initial e) = do
      a <- runReflexM initial 
      d <- holdDyn a (pushAlways runReflexM e)
      return (Updated a (updated d))
    
    switchM' e = return $ pushAlways runReflexM e 
          
    switchMapM (UpdatedMap initial e) = do
      a <- traverse runReflexM initial
      return $ UpdatedMap a $ 
        pushAlways (traverse $ traverse runReflexM) e
      
  
  
-- newtype PureHost t r a = PureHost
--   { unPureHost ::  StateT r (ReflexM t) a
--   }
-- 
-- deriving instance Reflex t => Functor (PureHost t r)
-- deriving instance Reflex t => Applicative (PureHost t r)
-- deriving instance Reflex t => Monad (PureHost t r)
-- deriving instance Reflex t => MonadHold t (PureHost t r)
-- deriving instance Reflex t => MonadSample t (PureHost t r)
-- deriving instance Reflex t => MonadFix (PureHost t r)  
-- 
--   
-- instance (Reflex t, Monoid r) => MonadWriter r (PureHost t r) where
--   tell r = PureHost $ modify' (r `mappend`) 
-- 
--   listen m = do
--     (a, r) <- liftPureHost (runPureHost m)
--     tell r
--     return (a, r)
--   
--   pass m = do
--     ((a, f), r) <- liftPureHost (runPureHost m)
--     tell (f r)
--     return a  
--   
--  
-- liftPureHost :: (Reflex t) => (forall m. (MonadReflex t m) => m a) -> PureHost t r a
-- liftPureHost ma = PureHost $ lift (M ma)
-- 
-- 
-- runPureHost :: (MonadReflex t m, Monoid r) => PureHost t r a -> m (a, r)
-- runPureHost app =  unM . flip runStateT mempty . unPureHost $ app
-- 
-- 
-- execPureHost :: (MonadReflex t m, Monoid r) => PureHost t r a -> m r
-- execPureHost app = snd <$> runPureHost app
--   
--   
-- instance (Reflex t, Monoid r) => MonadPerform t r (PureHost t r) where
--   
--   perform e = return $ pushAlways runPureHost e
--   collect m = liftPureHost (runPureHost m)
--   
--   
-- instance (Reflex t, Monoid s, Monoid r) => MapWriter (PureHost t) s r  where  
--   mapWriter f ms = do
--     (a, (r, b)) <- second f <$> liftPureHost (runPureHost ms)
--     tell r
--     return (a, b)
    

