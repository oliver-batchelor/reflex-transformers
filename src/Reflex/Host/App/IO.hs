{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TemplateHaskell #-}


module Reflex.Host.App.IO where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens hiding (Traversal)
import Data.Traversable

import Data.Semigroup.Applicative
import Data.Dependent.Sum
import Data.Maybe
import Reflex.Class hiding (constant)
import Reflex.Host.Class
import Reflex.Host.App.Class

import Data.IORef
import Data.Tuple

import Prelude


data EventChannels t = EventChannels
    -- | This is the channel to which external events should push their triggers.
    --
    -- Because this is a channel, there is no guarrante that the event that was pushed
    -- is fired directly in the next frame, as there can already be other events waiting
    -- which will be fired first.
  { envEventChan    :: Chan (AppInputs t)
  , envEventFrame   :: IORef [AppInputs t]
  }


type VoidActions t = Event t (Traversal (HostFrame t))    
  
data HostActions t = HostActions 
  { _hostPostBuild  :: HostFrame t ()
  , _hostActions    :: [VoidActions t]
  }  
  
  
$(makeLenses ''HostActions)    


newtype IOHost t  a = IOHost
  { unIOHost :: ReaderT (EventChannels t) (StateT (HostActions t) (HostFrame t))  a
  }

deriving instance ReflexHost t => Functor (IOHost t)
deriving instance ReflexHost t => Applicative (IOHost t)
deriving instance ReflexHost t => Monad (IOHost t)
deriving instance ReflexHost t => MonadHold t (IOHost t)
deriving instance ReflexHost t => MonadSample t (IOHost t)
deriving instance ReflexHost t => MonadReflexCreateTrigger t (IOHost t)
deriving instance (MonadIO (HostFrame t), ReflexHost t) => MonadIO (IOHost t)
deriving instance ReflexHost t => MonadFix (IOHost t)

deriving instance (MonadIO (HostFrame t), ReflexHost t) => MonadIOHost t (IOHost t)

-- | Run the application host monad in a reflex host frame and return the produced
-- application info.
runIOHostFrame :: (ReflexHost t) => EventChannels t -> IOHost t a -> HostFrame t (a, VoidActions t)
runIOHostFrame env app = do 
  (a, actions) <- flip runStateT initial . flip runReaderT env .  unIOHost $ app
  _hostPostBuild actions
  return (a, mconcat $ _hostActions actions)
    where initial = HostActions (return ()) []
  

  
execIOHostFrame :: (ReflexHost t)  =>  EventChannels t -> IOHost t a -> HostFrame t (VoidActions t)
execIOHostFrame env app = snd <$> runIOHostFrame env app



instance MonadIOHost t (IOHost t) => HasPostFrame t (IOHost t) where
  askPostFrame = IOHost $ do
    eventsFrameRef <- envEventFrame <$> ask
    return $ \e -> liftIO $ modifyIORef eventsFrameRef (e:)
  
instance  MonadIOHost t (IOHost t) => HasPostAsync t (IOHost t) where
  askPostAsync = IOHost $ do
    chan <- envEventChan <$> ask
    return $ liftIO . writeChan chan
  
instance  MonadIOHost t (IOHost t) => HasPostBuild t (IOHost t) where
  schedulePostBuild action = IOHost $ hostPostBuild %= (>>action)
      
instance  MonadIOHost t (IOHost t) => HasVoidActions t (IOHost t) where 
  performEvent_ event = IOHost $ hostActions %= ((Traversal <$> event):) 


readFrames :: (ReflexHost t, MonadIO m, MonadReflexHost t m) =>  EventChannels t -> m [DSum (EventTrigger t)]
readFrames env =  do
  performed <- liftIO $ atomicModifyIORef (envEventFrame env) (\a -> ([], a))
  runHostFrame $ concat <$> sequence performed
  
-- | Run an application. The argument is an action in the application host monad,
-- where events can be set up (for example by using 'newExteneralEvent').
--
-- This function will block until the application exits (when one of the 'eventsToQuit'
-- fires). 
hostApp :: (ReflexHost t, MonadIO m, MonadReflexHost t m) => IOHost t () -> m ()
hostApp app = do
  (chan, step) <- initHostApp app 
  forever $ liftIO (readChan chan) >>= step

-- | Initialize the application using a 'AppHost' monad. This function enables use
-- of use an external control loop. It returns a step function to step the application
-- based on external inputs received through the channel.
-- The step function returns False when one of the 'eventsToQuit' is fired.
initHostApp :: (ReflexHost t, MonadIO m, MonadReflexHost t m)
            => IOHost t () -> m (Chan (AppInputs t), AppInputs t -> m ())
initHostApp app = do
  env <- liftIO $ EventChannels <$> newChan <*> newIORef []
  
  actionsEvent <- runHostFrame $ execIOHostFrame env app
  nextActions <- subscribeEvent (getTraversal <$> actionsEvent)

  let
    go [] = return ()
    go triggers = do
      maybeAction <- fireEventsAndRead triggers $ eventValue nextActions 
      forM_ maybeAction $ \nextAction -> do
        runHostFrame nextAction
        go =<< readFrames env
        
    eventValue :: MonadReadEvent t m => EventHandle t a -> m (Maybe a)
    eventValue = readEvent >=> sequenceA

  go =<< readFrames env
  return (envEventChan env, go <=< runHostFrame)    
  


  
