{-# LANGUAGE ConstraintKinds #-}

module Reflex.Host.App.HostActions where

import Data.Dependent.Sum

import Reflex.Class hiding (constant)
import Reflex.Host.Class
import Reflex.Host.App.Util
import Reflex.Host.App.Switching
import Reflex.Host.App.Class

import Control.Monad
import Control.Lens

import Control.Monad.IO.Class
import Data.Semigroup.Applicative
import Data.Semigroup

import Data.Foldable
import qualified Data.Map.Strict as Map

-- import qualified  Data.DList  as DL
import Data.DList (DList)

import Prelude



type HostAction t = HostFrame t (DList (DSum (EventTrigger t)))
type ApHostAction t = Ap (HostFrame t) (DList (DSum (EventTrigger t)))

instance Semigroup (DList a)

data HostActions t = HostActions 
  { hostPerform   :: Events t (ApHostAction t) 
  , hostPostBuild :: ApHostAction t
  }  

instance ReflexHost t => Monoid (HostActions t) where
  mempty = HostActions mempty mempty
  mappend (HostActions p t) (HostActions p' t') = HostActions (mappend p p') (mappend t t')

instance ReflexHost t => Semigroup (HostActions t)

instance ReflexHost t => Switching t (HostActions t) where
    switching (HostActions perform postBuild) updated = do
      updatedPerform <- switching perform (hostPerform <$> updated)
      return (HostActions (updatedPostBuild <> updatedPerform) postBuild)
      
      where
        updatedPostBuild = events (hostPostBuild <$> updated)
      

  
instance (ReflexHost t) => SwitchMerge t (HostActions t) where
  switchMerge initial updates = do 

    updatedPerform <- switchMerge perform updates'
    return (HostActions (updatedPostBuild <> updatedPerform) postBuild)
  
    where
      perform = hostPerform <$> initial
      updates' = fmap (fmap hostPerform) <$> updates
    
      postBuild = fold $ hostPostBuild <$> initial
      updatedPostBuild = events (fold . Map.mapMaybe (fmap hostPostBuild) <$> updates)
    
      
      
      
{-# INLINEABLE makePerform_ #-}
makePerform_ :: ReflexHost t => Event t (HostFrame t ()) -> HostActions t
makePerform_ e = mempty { hostPerform = events $ Ap . fmap (const mempty) <$> e }

{-# INLINEABLE makePerform #-}
makePerform :: ReflexHost t => Event t (HostFrame t (DList (DSum (EventTrigger t)))) -> HostActions t
makePerform e = mempty { hostPerform = events $ Ap <$> e }

{-# INLINEABLE makePostBuild #-}
makePostBuild :: ReflexHost t => HostFrame t (DList (DSum (EventTrigger t))) -> HostActions t
makePostBuild pb = mempty { hostPostBuild = Ap pb }



{-# INLINEABLE mergeHostActions #-}
mergeHostActions :: (ReflexHost t) => Events t (ApHostAction t) -> Event t (HostAction t)
mergeHostActions e = getApp <$> mergeEvents e


  
class HasHostActions t r | r -> t where
  actions :: Lens' r (HostActions t)
 
instance HasHostActions t (HostActions t) where

  actions = lens id const
  
{-# INLINEABLE tellActions #-}
tellActions :: (ReflexHost t, MonadWriter r m, HasHostActions t r) => HostActions t -> m ()
tellActions a = tell (mempty & actions .~ a)

performActions_ :: (ReflexHost t, MonadWriter r m, HasHostActions t r) =>  Event t (HostFrame t ()) -> m ()
performActions_  = tellActions . makePerform_

scheduleActions :: (IOHost t m, MonadWriter r m, HasHostActions t r) => HostFrame t a -> m (Event t a)
scheduleActions actions = do 
  (event, construct) <- newEventWithConstructor
  tellActions . makePostBuild $ liftIO . construct =<< actions
  return event

scheduleActions_ :: (ReflexHost t, MonadWriter r m, HasHostActions t r) => HostFrame t () -> m ()
scheduleActions_ action = tellActions . makePostBuild $ action >> pure mempty
  
  
performActions :: (IOHost t m,  MonadWriter r m, HasHostActions t r) =>  Event t (HostFrame t a) -> m (Event t a)
performActions e = do 
  (event, construct) <- newEventWithConstructor
  tellActions . makePerform $ (liftIO . construct =<<) <$> e
  return event
  

 


