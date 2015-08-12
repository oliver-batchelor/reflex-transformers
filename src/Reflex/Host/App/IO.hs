{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}  -- For deriving MonadReflexCreateTrigger


module Reflex.Host.App.IO where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Lens hiding (Traversal)
import Data.Dependent.Sum
import Data.IORef
import Data.Bifunctor

import Reflex.Class hiding (constant)
import Reflex.Host.Class

import Data.Semigroup.Applicative

import Reflex.Host.App.Class
import Reflex.Host.App
import qualified  Data.DList  as DL
import Data.DList (DList)

import Prelude


newtype IOHost t r a = IOHost
  { unIOHost :: ReaderT (Chan (AppInputs t)) (StateT r (HostFrame t))  a
  }

  
deriving instance ReflexHost t => Functor (IOHost t r)
deriving instance ReflexHost t => Applicative (IOHost t r)
deriving instance ReflexHost t => Monad (IOHost t r)
deriving instance ReflexHost t => MonadHold t (IOHost t r)
deriving instance ReflexHost t => MonadSample t (IOHost t r)
deriving instance ReflexHost t => MonadReflexCreateTrigger t (IOHost t r)
deriving instance (MonadIO (HostFrame t), ReflexHost t) => MonadIO (IOHost t r)
deriving instance ReflexHost t => MonadFix (IOHost t r)

-- instance MonadSample t m => MonadSample t (StateT s m) where
--   sample = lift . sample
-- 
-- instance MonadHold t m => MonadHold t (StateT s m) where
--   hold init = lift . hold init  

-- | Run the application host monad in a reflex host frame and return the produced
-- application info.

{-# INLINEABLE runIOHostFrame #-}
runIOHostFrame :: (ReflexHost t, Monoid r) => Chan (AppInputs t) -> IOHost t r a -> HostFrame t (a, r)
runIOHostFrame env app = flip runStateT mempty . flip runReaderT env . unIOHost $ app

{-# INLINEABLE execIOHostFrame #-}
execIOHostFrame :: (ReflexHost t, Monoid r) => Chan (AppInputs t) -> IOHost t r a -> HostFrame t r
execIOHostFrame env app = snd <$> runIOHostFrame env app

{-# INLINEABLE liftHostFrame #-}
liftHostFrame :: ReflexHost t => HostFrame t a -> IOHost t r a
liftHostFrame = IOHost . lift . lift
 
 
instance (Monoid r, ReflexHost t, HostHasIO t (IOHost t r)) => HostWriter r (IOHost t r) where
  
  {-# INLINEABLE tellHost #-}
  tellHost r = IOHost $ modify (r `mappend`) 
  
  {-# INLINEABLE collectHost #-}
  collectHost ma  = do
    env <- IOHost ask
    liftHostFrame $ runIOHostFrame env ma

instance (ReflexHost t, HostHasIO t (IOHost t r), Monoid s, Monoid r) => HostMap (IOHost t) s r  where  
  mapHost f ms = do
    env <- IOHost ask
    (a, (r, b)) <- second f <$> liftHostFrame (runIOHostFrame env ms)
    tellHost r
    return (a, b)    
    

    
instance HostHasIO t (IOHost t r) => HasPostAsync t (IOHost t r) where
  askPostAsync = IOHost $ do
    chan <- ask
    return $ liftIO . writeChan chan    
  
instance (MonadIO (HostFrame t), Switchable t r, Monoid r, ReflexHost t, HasHostActions t r) 
        => MonadAppHost t r (IOHost t r) where
          
  type Host t (IOHost t r) = HostFrame t
  
  {-# INLINEABLE performHost #-}
  performHost = performEvent
   
  {-# INLINEABLE askRunAppHost #-}
  askRunAppHost = IOHost $ do
    env <- ask
    return (runIOHostFrame env)
   
  {-# INLINEABLE liftAppHost #-}
  liftAppHost = liftHostFrame
  
    

instance (MonadIO (HostFrame t), ReflexHost t) => HostHasIO t (IOHost t r) 
instance (HasHostActions t r, HostHasIO t (IOHost t r), MonadAppHost t r (IOHost t r)) => MonadIOHost t r (IOHost t r)
  
  
  
-- | Run an application. The argument is an action in the application host monad,
-- where events can be set up (for example by using 'newExteneralEvent').
--
-- This function will block until the application exits (when one of the 'eventsToQuit'
-- fires).
  
hostApp :: (ReflexHost t, MonadIO m, MonadReflexHost t m) => IOHost t (HostActions t) () -> m ()
hostApp app = loop =<< initHostApp app 
  
  
  where
    loop (chan, step) = do
      x <- liftIO (readChan chan) >>= runHostFrame
      unless (null x) $ step x >> loop (chan, step)
     

-- | Initialize the application using a 'AppHost' monad. This function enables use
-- of use an external control loop. It returns a step function to step the application
-- based on external inputs received through the channel.
-- The step function returns False when one of the 'eventsToQuit' is fired.
initHostApp :: (ReflexHost t, MonadIO m, MonadReflexHost t m)
            => IOHost t (HostActions t) () -> m (Chan (AppInputs t), [DSum (EventTrigger t)] -> m ())
initHostApp app = do
  env <- liftIO newChan 
  
  (HostActions performUpdated performInit) <- runHostFrame $ execIOHostFrame env app
  nextActionEvent <- subscribeEvent (mergeHostActions performUpdated)

  let
    go [] = return ()
    go triggers = do
      maybeAction <- fireEventsAndRead triggers $ eventValue nextActionEvent 
      forM_ maybeAction $ \nextAction -> do
        go =<< DL.toList <$> runHostFrame nextAction
        
    eventValue :: MonadReadEvent t m => EventHandle t a -> m (Maybe a)
    eventValue = readEvent >=> sequenceA

  go =<< DL.toList <$> runHostFrame (getApp performInit)
  return (env, go)

   
  


