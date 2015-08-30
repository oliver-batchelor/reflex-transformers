{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}  -- For deriving MonadReflexCreateTrigger


module Reflex.Host.App.AppHost 
  ( AppHost
  , hostApp
  , initHostApp
  
  , module  Reflex.Host.App.HostActions
  
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Dependent.Sum
import Data.Bifunctor

import qualified  Data.DList  as DL
import Data.Semigroup.Applicative

import Reflex.Class hiding (constant)
import Reflex.Host.Class
import Reflex.Host.App.Class
import Reflex.Host.App.HostActions

import Prelude


type AppInputs t = HostFrame t [DSum (EventTrigger t)]

newtype AppHost t r a = AppHost
  { unAppHost :: ReaderT (Chan (AppInputs t)) (StateT r (HostFrame t))  a
  }

   
deriving instance ReflexHost t => Functor (AppHost t r)
deriving instance ReflexHost t => Applicative (AppHost t r)
deriving instance ReflexHost t => Monad (AppHost t r)
deriving instance ReflexHost t => MonadHold t (AppHost t r)
deriving instance ReflexHost t => MonadSample t (AppHost t r)
deriving instance ReflexHost t => MonadReflexCreateTrigger t (AppHost t r)
deriving instance (MonadIO (HostFrame t), ReflexHost t) => MonadIO (AppHost t r)
deriving instance ReflexHost t => MonadFix (AppHost t r)



{-# INLINEABLE runAppHostFrame #-}
runAppHostFrame :: (ReflexHost t, Monoid r) => Chan (AppInputs t) -> AppHost t r a -> HostFrame t (a, r)
runAppHostFrame env app = flip runStateT mempty . flip runReaderT env . unAppHost $ app

{-# INLINEABLE execAppHostFrame #-}
execAppHostFrame :: (ReflexHost t, Monoid r) => Chan (AppInputs t) -> AppHost t r a -> HostFrame t r
execAppHostFrame env app = snd <$> runAppHostFrame env app

{-# INLINEABLE liftHostFrame #-}
liftHostFrame :: ReflexHost t => HostFrame t a -> AppHost t r a
liftHostFrame = AppHost . lift . lift
  
 
collectAppHost :: (ReflexHost t, Monoid r) => AppHost t r a -> AppHost t s (a, r)
collectAppHost m = do
    env <- AppHost ask
    liftHostFrame $ runAppHostFrame env m
 
 
instance (Monoid r, ReflexHost t) => MonadWriter r (AppHost t r) where
  
  tell r = AppHost $ modify' (r `mappend`) 
  listen m = do
    (a, r) <- collectAppHost m
    tell r
    return (a, r)
  
  pass m = do
    ((a, f), r) <- collectAppHost m
    tell (f r)
    return a
  
 
 
instance (ReflexHost t, Monoid s, Monoid r) => MapWriter (AppHost t) s r  where  
  mapWriter f m = do
    (a, (r, b)) <- second f <$> collectAppHost m
    tell r
    return (a, b)    
    


  
instance (SwitchMerge t r, MonadIO (HostFrame t), Monoid r, ReflexHost t, HasHostActions t r) 
        => MonadAppHost t r (AppHost t r) where
          
  type Host t (AppHost t r) = HostFrame t
  
  performEvent = performActions
   
  askRunAppHost = AppHost $ do
    env <- ask
    return (runAppHostFrame env)
   
  liftHost = liftHostFrame
  
    

instance (ReflexHost t, HasHostActions t r, MonadIO (HostFrame t), MonadAppHost t r (AppHost t r)) 
         => MonadIOHost t r (AppHost t r) where
           
    askPostAsync = AppHost $ do
      chan <- ask
      return $ liftIO . writeChan chan    
      
    performEvent_ = performActions_
    
    schedulePostBuild_ = scheduleActions_
    
    schedulePostBuild = scheduleActions
  
  
-- | Run an application. The argument is an action in the application host monad,
-- where events can be set up (for example by using 'newExteneralEvent').
--
-- This function will block until the application exits (when one of the 'eventsToQuit'
-- fires).
  
hostApp :: (ReflexHost t, MonadIO m, MonadReflexHost t m) => AppHost t (HostActions t) () -> m ()
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
            => AppHost t (HostActions t) () -> m (Chan (AppInputs t), [DSum (EventTrigger t)] -> m ())
initHostApp app = do
  env <- liftIO newChan 
  
  (HostActions perform postBuild) <- runHostFrame $ execAppHostFrame env app
  nextActionEvent <- subscribeEvent (mergeHostActions perform)

  let
    go [] = return ()
    go triggers = do
      maybeAction <- fireEventsAndRead triggers $ eventValue nextActionEvent 
      forM_ maybeAction $ \nextAction -> do
        go =<< DL.toList <$> runHostFrame nextAction
        
    eventValue :: MonadReadEvent t m => EventHandle t a -> m (Maybe a)
    eventValue = readEvent >=> sequenceA

  go =<< DL.toList <$> runHostFrame (getApp postBuild)
  return (env, go)

   
  


