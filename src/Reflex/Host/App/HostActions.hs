module Reflex.Host.App.HostActions where

import Data.Dependent.Sum

import Reflex.Class hiding (constant)
import Reflex.Host.Class

import Reflex.Host.App.Class 
import Reflex.Host.App.Util

import Control.Monad
import Control.Monad.IO.Class
import Data.Semigroup.Applicative
import Data.Semigroup


import qualified  Data.DList  as DL
import Data.DList (DList)

import Prelude


import Reflex.Host.App.Util

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


instance ReflexHost t => Switching t (HostActions t) where
    switching (HostActions perform postBuild) updated = do
      updatedPerform <- switching perform (hostPerform <$> updated)
      return (HostActions (updatedPostBuild `mappend` updatedPerform) postBuild)
      
      where
        updatedPostBuild = events (hostPostBuild <$> updated)
      

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
  fromActions :: HostActions t -> r
 
instance HasHostActions t (HostActions t) where
  {-# INLINE fromActions #-}
  fromActions = id
  
  
tellActions :: (ReflexHost t, MonadAppWriter r m, HasHostActions t r) => HostActions t -> m ()
tellActions = tellApp . fromActions

performActions_ :: (ReflexHost t, MonadAppWriter r m, HasHostActions t r) =>  Event t (HostFrame t ()) -> m ()
performActions_  = tellActions . makePerform_

scheduleActions :: (ReflexHost t, MonadAppWriter r m, HasHostActions t r) => HostFrame t (DList (DSum (EventTrigger t))) -> m ()  
scheduleActions = tellActions . makePostBuild

scheduleActions_ :: (ReflexHost t, MonadAppWriter r m, HasHostActions t r) => HostFrame t () -> m ()
scheduleActions_ action = scheduleActions (action >> pure mempty)
  
performActions :: (ReflexHost t, MonadReflexCreateTrigger t m, MonadIO m, MonadIO (HostFrame t), 
                  MonadAppWriter r m, HasHostActions t r) =>  Event t (HostFrame t a) -> m (Event t a)
performActions e = do 
  (event, construct) <- newEventWithConstructor
  tellActions . makePerform $ (\h -> h >>= liftIO . construct) <$> e
  return event
  

 


