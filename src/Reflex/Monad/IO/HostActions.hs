{-# LANGUAGE ConstraintKinds #-}

module Reflex.Monad.IO.HostActions where

import Data.Dependent.Sum

import Reflex.Class hiding (constant)
import Reflex.Host.Class
import Reflex.Monad.IO


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
    switching (HostActions toPerform postBuild) updates = do
      updatedPerform <- switching toPerform (hostPerform <$> updates)
      return (HostActions (updatedPostBuild <> updatedPerform) postBuild)
      
      where
        updatedPostBuild = events (hostPostBuild <$> updates)
      

  
instance (ReflexHost t) => SwitchMerge t (HostActions t) where
  switchMerge initial updates = do 

    updatedPerform <- switchMerge toPerform updates'
    return (HostActions (updatedPostBuild <> updatedPerform) postBuild)
  
    where
      toPerform = hostPerform <$> initial
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
mergeHostActions e = getAp <$> mergeEvents e


  
class HasHostActions t r | r -> t where
  actions :: Lens' r (HostActions t)
 
instance HasHostActions t (HostActions t) where

  actions = lens id (const id)
  
{-# INLINEABLE tellActions #-}
tellActions :: (ReflexHost t, MonadWriter r m, HasHostActions t r) => HostActions t -> m ()
tellActions a = tell (mempty & actions .~ a)

performActions_ :: (ReflexHost t, MonadWriter r m, HasHostActions t r) =>  Event t (HostFrame t ()) -> m ()
performActions_  = tellActions . makePerform_

scheduleActions :: (MonadReflexIO t m, MonadWriter r m, HasHostActions t r) => HostFrame t a -> m (Event t a)
scheduleActions a = do 
  (event, construct) <- newEventWithConstructor
  tellActions . makePostBuild $ liftIO . construct =<< a
  return event

scheduleActions_ :: (ReflexHost t, MonadWriter r m, HasHostActions t r) => HostFrame t () -> m ()
scheduleActions_ action = tellActions . makePostBuild $ action >> pure mempty
  
  
performActions :: (MonadReflexIO t m,  MonadWriter r m, HasHostActions t r) =>  Event t (HostFrame t a) -> m (Event t a)
performActions e = do 
  (event, construct) <- newEventWithConstructor
  tellActions . makePerform $ (liftIO . construct =<<) <$> e
  return event
  

 


