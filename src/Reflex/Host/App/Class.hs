{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Host.App.Class where

import Data.Dependent.Sum

import Reflex.Class hiding (constant)
import Reflex.Host.Class


import Control.Monad
import Control.Monad.State.Strict
import Data.Semigroup.Applicative
import Data.Semigroup
import Data.Maybe
import Data.Foldable
import Data.IORef

import qualified  Data.DList  as DL
import Data.DList (DList)

import Prelude


type AppInputs t = HostFrame t [DSum (EventTrigger t)]


class (Reflex t) => Switching t r | r -> t where
   -- | Generalization of switchable reactive types (e.g. Event, Behavior)
   switching :: MonadHold t m => r -> Event t r -> m r
   
class (Switching t r, Monoid r) => SwitchMerge t r | r -> t where   
   switchMerge :: (MonadHold t m, Ord k) => Map k r -> Event t (Map k (Maybe r)) -> m r


instance (Switching t a, Switching t b) => Switching t (a, b) where
  switching (a, b) e = liftM2 (,) (switching a $ fmap fst <$> e) (switching b $ fmap snd <$> e)
  
  
instance (SwitchMerge t a, SwitchMerge t b) => SwitchMerge t (a, b) where
  switchMerge (a, b) e = do
    liftM2 (,) (switchMerge a $ fst <$> e) (switchMerge b $ snd <$> e)


newtype Behaviors t a = Behaviors { unBehaviors :: [Behavior t a] } deriving Monoid
newtype Events t a = Events { unEvents :: DList [Event t a] } deriving Monoid


events :: Event t a -> Events t a
events = Events . pure

behaviors :: Behavior t a -> Behaviors t a
behaviors = Behaviors . pure

instance (Monoid a, Reflex t) => SwitchGroup t [Behaviors t a]  where
  switchGroup bs updated = behaviors <$> switcher (mergeGroup bs) (mergeGroup <$> updated)
  mergeGroup = events . mconcat . unBehaviors

  
      
instance (Semigroup a, Reflex t) => SwitchGroup t (Events t a) where
  switchGroup es updated = events <$> switchPromptly (mergeGroup es) (mergeGroup <$> updated)
  mergeGroup = events . mconcat . unEvents
  


  
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
holdHost initial updated = tellHost =<< switchGroup initial updated

holdHostF :: (MonadHold t m,  HostWriter r m, Switchable t r, Foldable f) => f r -> Event t (f r) -> m ()
holdHostF initial updated = tellHost =<< switchGroup (fold initial) (fold <$> updated)

  
class (Reflex t, MonadFix m, MonadHold t m, MonadHold t (Host t m), MonadFix (Host t m),  
       HostWriter r m, Switchable t r) => MonadAppHost t r m | m -> t r where
  type Host t m :: * -> *
    
  -- | Run a monadic action after each frame in which the event fires, and return the result
  -- in a new event which is fired immediately following the frame in which the original
  -- event fired.
  
  performHost :: Event t (Host t m a) -> m (Event t a)
  
  askRunAppHost :: m (m a -> Host t m (a, r)) 
  
  liftAppHost :: Host t m a -> m a
  
  

  
-- deriving creates an error requiring ImpredicativeTypes
instance (Reflex t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (StateT s m) where
  newEventWithTrigger initializer = lift $ newEventWithTrigger initializer
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer
  
