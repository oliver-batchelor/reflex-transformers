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
import Data.Maybe


import Prelude


type AppInputs t = HostFrame t [DSum (EventTrigger t)]


class (Reflex t) => Switchable t r | r -> t where
   -- | Generalization of switchable reactive types (e.g. Event, Behavior)
   genericSwitch :: MonadHold t m => r -> Event t r -> m r


instance (Monoid a, Reflex t) => Switchable t (Behavior t a)  where
  genericSwitch = switcher
      
instance (Reflex t) => Switchable t (Event t a) where
  genericSwitch = switchPromptly


instance (Switchable t a, Switchable t b) => Switchable t (a, b) where
  genericSwitch (a, b) e = liftM2 (,) (genericSwitch a $ fst <$> e) (genericSwitch b $ snd <$> e)

  
  
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



  
class (Reflex t, MonadHold t m, HostWriter r m, Switchable t r) => MonadAppHost t r m | m -> t r where
  
  -- | Run a monadic action in an event, returning the value and writer result
  performHost :: Event t (m a) -> m (Event t (a, r))
  
  -- | Provided purely for performance reasons, run this reactive code at the lowest
  -- level providing MonadHold, MonadFix. To avoid needlessly running Pure AppHosts on
  -- top of a stack of transformers.
  liftHold :: (forall n. (MonadHold t n, MonadFix n) => n a) -> m a
  

  
  
-- hostEmbed :: (HostSwitch t m r, MonadIOHost t m' r')  => (r' -> r) -> m' r' a -> m r a
-- hostEmbed f inner = do
  


  
class (ReflexHost t, MonadIO m, MonadIO (HostFrame t), MonadFix (HostFrame t), MonadReflexCreateTrigger t m) 
      => HostHasIO t m | m -> t where
  
  -- | Lift a HostFrame to run in an IO based host
  liftHostFrame :: HostFrame t a -> m a
  
  
-- class MonadIOHost   
  
  
newtype HostActions t = HostActions { unHostAction ::  Event t (Traversal (HostFrame t)) }  
      
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
performEvent_ e = tellHost . fromActions . HostActions $ Traversal <$> e
      
 
  
-- deriving creates an error requiring ImpredicativeTypes
instance (Reflex t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (StateT s m) where
  newEventWithTrigger initializer = lift $ newEventWithTrigger initializer
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer