{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Host.App.Class where

import Data.Dependent.Sum

import Reflex.Class hiding (constant)
import Reflex.Host.Class
-- import Reflex

import Control.Monad
import Control.Monad.State.Strict
import Data.Semigroup.Applicative
import Data.Semigroup
import Data.Maybe
import Data.Foldable

import Prelude


type AppInputs t = HostFrame t [DSum (EventTrigger t)]


class (Reflex t) => Switchable t r | r -> t where
   -- | Generalization of switchable reactive types (e.g. Event, Behavior)
   genericSwitch :: MonadHold t m => r -> Event t r -> m r


instance (Switchable t a, Switchable t b) => Switchable t (a, b) where
  genericSwitch (a, b) e = liftM2 (,) (genericSwitch a $ fst <$> e) (genericSwitch b $ snd <$> e)


newtype Behaviors t a = Behaviors { unBehaviors :: [Behavior t a] } deriving Monoid
newtype Events t a = Events { unEvents :: [Event t a] } deriving Monoid

instance (Monoid a, Reflex t) => Switchable t (Behaviors t a)  where
  genericSwitch bs updated = Behaviors . pure <$> switcher (mergeBehaviors bs) (mergeBehaviors <$> updated)
      
instance (Semigroup a, Reflex t) => Switchable t (Events t a) where
  genericSwitch es updated = Events . pure <$> switchPromptly (mergeEvents es) (mergeEvents <$> updated)
  
  
mergeEvents :: (Reflex t, Semigroup a) => Events t a -> Event t a
mergeEvents = mconcat . unEvents

mergeBehaviors :: (Reflex t, Monoid a) => Behaviors t a -> Behavior t a
mergeBehaviors = mconcat . unBehaviors

  
class (Monad m, Monoid r) => HostWriter r m | m -> r  where  
 
  -- | Writes 'r' to the host, analogous to 'tell' from MonadWriter
  tellHost :: r -> m ()
  
  -- | Collect the result of one writer and return it in another
  collectHost :: m a -> m (a, r)
  

class (HostWriter r (m r), HostWriter s (m s)) => HostMap m s r  where  
  
  -- | Embed one HostWriter in another, a function is used to split the 
  --   result of the inner writer into parts to 'tell' the outer writer
  --   and a part to return.
  mapHost :: (s -> (r, b)) -> m s a -> m r (a, b) 
  
  
appendHost :: HostMap m (r, s) r => m (r, s) a -> m r (a, s)
appendHost = mapHost id

holdHost :: (MonadHold t m, HostWriter r m, Switchable t r) => r -> Event t r -> m ()
holdHost initial updated = tellHost =<< genericSwitch initial updated

holdHostF :: (MonadHold t m,  HostWriter r m, Switchable t r, Foldable f) => f r -> Event t (f r) -> m ()
holdHostF initial updated = tellHost =<< genericSwitch (fold initial) (fold <$> updated)

  
class (Reflex t, MonadFix m, MonadHold t m, MonadHold t (Host t m), MonadFix (Host t m),  
       HostWriter r m, Switchable t r) => MonadAppHost t r m | m -> t r where
  type Host t m :: * -> *
    
  -- | Run a monadic action after each frame in which the event fires, and return the result
  -- in a new event which is fired immediately following the frame in which the original
  -- event fired.
  
  performEvent :: Event t (Host t m a) -> m (Event t a)
  
  askRunAppHost :: m (m a -> Host t m (a, r)) 
  
  liftAppHost :: Host t m a -> m a
  
  
class (ReflexHost t, MonadIO m, MonadIO (HostFrame t), MonadFix (HostFrame t), 
       MonadReflexCreateTrigger t m) => HostHasIO t m | m -> t 
  
  
newtype HostActions t = HostActions { unHostAction ::  Events t (Traversal (HostFrame t)) }  deriving Monoid
      
hostAction :: Reflex t => Event t (HostFrame t ()) -> HostActions t
hostAction e = HostActions $ Events [Traversal <$> e]
      
mergeHostActions :: (ReflexHost t) => HostActions t -> Event t (HostFrame t ())
mergeHostActions (HostActions e) = getTraversal <$> mergeEvents e

      
class (HostHasIO t m) => HasPostFrame t m | m -> t where
  askPostFrame :: m (AppInputs t -> IO ())
  
class (HostHasIO t m) => HasPostAsync t m | m -> t where
  askPostAsync :: m (AppInputs t -> IO ())
  
class (HostHasIO t m) => HasPostBuild t m | m -> t where
  schedulePostBuild :: HostFrame t () -> m ()
  
class HasHostActions t r | r -> t where
  fromActions :: HostActions t -> r
 
instance HasHostActions t (HostActions t) where
  fromActions = id
 
 
performEvent_ :: (Reflex t, HostWriter r m, HasHostActions t r) =>  Event t (HostFrame t ()) -> m ()
performEvent_  = tellHost . fromActions . hostAction
      
class (HasPostFrame t m, HasPostAsync t m, HasPostBuild t m, HasHostActions t r, MonadAppHost t r m) => MonadIOHost t r m | m -> t r
  
-- deriving creates an error requiring ImpredicativeTypes
instance (Reflex t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (StateT s m) where
  newEventWithTrigger initializer = lift $ newEventWithTrigger initializer
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer