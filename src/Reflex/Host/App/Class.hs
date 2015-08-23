{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Host.App.Class where


import Data.Dependent.Sum

import Reflex.Class hiding (constant)
import Reflex.Host.Class
import Reflex.Host.App.Util

import Data.Map (Map)

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


type AppInputs t m = Host t m [DSum (EventTrigger t)]


class (Reflex t) => Switching t r | r -> t where
   -- | Generalization of switchable reactive types (e.g. Event, Behavior)
   switching :: MonadHold t m => r -> Event t r -> m r
   
class (Switching t r, Monoid r) => SwitchMerge t r | r -> t where   
   switchMerge :: (MonadHold t m, Ord k) => Map k r -> Event t (Map k (Maybe r)) -> m r


instance (Switching t a, Switching t b) => Switching t (a, b) where
  switching (a, b) e = liftM2 (,) (switching a $ fst <$> e) (switching b $ snd <$> e)
  
  
instance (SwitchMerge t a, SwitchMerge t b) => SwitchMerge t (a, b) where
  switchMerge initial e = do
    a <- switchMerge (fst <$> initial) (fmap (fmap fst) <$> e)
    b <- switchMerge (snd <$> initial) (fmap (fmap snd) <$> e)
    return (a, b)


newtype Behaviors t a = Behaviors { unBehaviors :: [Behavior t a] } deriving Monoid
newtype Events t a = Events { unEvents :: [Event t a] } deriving Monoid


events :: Event t a -> Events t a
events = Events . pure


mergeEvents :: (Reflex t, Semigroup a) => Events t a -> Event t a
mergeEvents = mergeWith (<>) . unEvents

behaviors :: Behavior t a -> Behaviors t a
behaviors = Behaviors . pure

mergeBehaviors :: (Reflex t, Monoid a) => Behaviors t a -> Behavior t a
mergeBehaviors = mconcat . unBehaviors



instance (Monoid a, Reflex t) => Switching t (Behaviors t a)  where
  switching bs updates = behaviors <$> switcher (mergeBehaviors bs) (mergeBehaviors <$> updates)

instance (Semigroup a, Reflex t) => Switching t (Events t a) where
  switching es updates = events <$> switchPromptly (mergeEvents es) (mergeEvents <$> updates)

  
class (Monad m, Monoid r) => MonadAppWriter r m | m -> r  where  
 
  -- | Writes 'r' to the host, analogous to 'tell' from MonadWriter
  tellApp :: r -> m ()
  
  -- | Collect the result of one writer and return it in another
  collectApp :: m a -> m (a, r)
  

class (MonadAppWriter r (m r), MonadAppWriter s (m s)) => MapWriter m s r  where  
  
  -- | Embed one MonadAppWriter in another, a function is used to split the 
  --   result of the inner writer into parts to 'tell' the outer writer
  --   and a part to return.
  mapWriter :: (s -> (r, b)) -> m s a -> m r (a, b) 
  
  
appendApp :: MapWriter m (r, s) r => m (r, s) a -> m r (a, s)
appendApp = mapWriter id

holdApp :: (MonadHold t m, MonadAppWriter r m, Switching t r) => r -> Event t r -> m ()
holdApp initial updated = tellApp =<< switching initial updated

holdAppF :: (MonadHold t m,  MonadAppWriter r m, Switching t r, Foldable f) => f r -> Event t (f r) -> m ()
holdAppF initial updated = tellApp =<< switching (fold initial) (fold <$> updated)

  
class (ReflexHost t, MonadFix m, MonadHold t m, MonadHold t (Host t m), MonadFix (Host t m),  
       MonadAppWriter r m, Switching t r) => MonadAppHost t r m | m -> t r where
  type Host t m :: * -> *
    
  -- | Run a monadic host action during or immediately each frame in which the event fires, 
  -- and return the result in an event fired before other events. 
  -- (Either in the same frame, or in the next immediate frame.
  performEvent :: Event t (Host t m a) -> m (Event t a)
  
  -- | Return a funtion to run a MonadApp action in it's Host 
  -- return it's MonadAppWriter contents.
  
  askRunApp :: m (m a -> Host t m (a, r)) 
  
  -- | Lift a Host action to a MonadApp action
  liftHost :: Host t m a -> m a
  

class (MonadAppHost t r m, MonadIO m, MonadIO (Host t m), 
      MonadReflexCreateTrigger t m) => MonadIOHost t r m | m -> t r  where
        
  -- | Return a function to post events via IO to a fifo Event queue.
  askPostAsync :: m (AppInputs t m -> IO ())      

  -- | Run a monadic Host action for the purposes of it's IO effects. 
  -- e.g. for setting properties of an underlying UI or executing an XHR request.
  performEvent_ :: Event t (Host t m a) -> m ()

  -- | Run host action for it's effects after construction.
  schedulePostBuild :: Host t m () -> m ()  
  
  -- | Run host action for it's effects after construction, 
  -- return the result in an Event.
  generatePostBuild :: Host t m a -> m (Event t a)
  
  
-- deriving creates an error requiring ImpredicativeTypes
instance (Reflex t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (StateT s m) where
  newEventWithTrigger initializer = lift $ newEventWithTrigger initializer
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer
  
