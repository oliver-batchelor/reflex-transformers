{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}  -- For deriving MonadReflexCreateTrigger


module Reflex.Host.App.IO where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens hiding (Traversal)
import Data.IORef

import Reflex.Class hiding (constant)
import Reflex.Host.Class

import Reflex.Host.App.Class
import Reflex.Host.App





import Prelude


data EventChannels t = EventChannels
    -- | This is the channel to which external events should push their triggers.
    -- These events are processed in a FIFO queue after any internal events are fired,
    -- they will not necessarily occur in the frame after they were fired.
    
  { envEventChan    :: Chan (AppInputs t)
  
    -- | Internal events should push triggers to this list istead.
    -- these triggers will be gathered up and fired immediately in the next frame 
  , envEventFrame   :: IORef [AppInputs t]
  }


  
data HostState t r = HostState 
  { _hostPostBuild  :: HostFrame t ()
  , _hostActions    :: [r]
  }  
  
  
$(makeLenses ''HostState)    


newtype IOHost t r a = IOHost
  { unIOHost :: ReaderT (EventChannels t) (StateT (HostState t r) (HostFrame t))  a
  }

deriving instance ReflexHost t => Functor (IOHost t r)
deriving instance ReflexHost t => Applicative (IOHost t r)
deriving instance ReflexHost t => Monad (IOHost t r)
deriving instance ReflexHost t => MonadHold t (IOHost t r)
deriving instance ReflexHost t => MonadSample t (IOHost t r)
deriving instance ReflexHost t => MonadReflexCreateTrigger t (IOHost t r)
deriving instance (MonadIO (HostFrame t), ReflexHost t) => MonadIO (IOHost t r)
deriving instance ReflexHost t => MonadFix (IOHost t r)

 
instance (Monoid r, ReflexHost t, HostHasIO t (IOHost t r)) => HostWriter r (IOHost t r) where
  tellHost r = IOHost $ hostActions %= (r:) 
  collectHost ma  = do
    env <- IOHost ask
    liftHostFrame $ runIOHostFrame env ma

  
instance (MonadIO (HostFrame t), Switchable t r, Monoid r, ReflexHost t, HasHostActions t r) => MonadAppHost t r (IOHost t r) where
  performHost e = do 
    env <- IOHost ask
    performEvent $ runIOHostFrame env <$> e 
  
  
instance HostHasIO t (IOHost t r) => HasPostFrame t (IOHost t r) where
  askPostFrame = IOHost $ do
    eventsFrameRef <- envEventFrame <$> ask
    return $ \e -> liftIO $ modifyIORef eventsFrameRef (e:)

    
instance HostHasIO t (IOHost t r) => HasPostAsync t (IOHost t r) where
  askPostAsync = IOHost $ do
    chan <- envEventChan <$> ask
    return $ liftIO . writeChan chan
    
    

instance (MonadIO (HostFrame t), ReflexHost t) => HostHasIO t (IOHost t r) where
  liftHostFrame = IOHost . lift . lift
  
  
instance  HostHasIO t (IOHost t r) => HasPostBuild t (IOHost t r) where
  schedulePostBuild action = IOHost $ hostPostBuild %= (>>action)  
  
  

-- | Run the application host monad in a reflex host frame and return the produced
-- application info.
runIOHostFrame :: (ReflexHost t, Monoid r) => EventChannels t -> IOHost t r a -> HostFrame t (a, r)
runIOHostFrame env app = do 
  (a, actions) <- flip runStateT initial . flip runReaderT env . unIOHost $ app
  _hostPostBuild actions
  return (a, mconcat $ _hostActions actions)
    where initial = HostState (return ()) []
  
execIOHostFrame :: (ReflexHost t, Monoid r) => EventChannels t -> IOHost t r a -> HostFrame t r
execIOHostFrame env app = snd <$> runIOHostFrame env app

{-

    


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
  -}


