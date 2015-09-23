{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Reflex.Host.App.Class
  ( MonadWriter (..)
  , MapWriter(..)
  , censor
  , MonadAppHost (..)
  , MonadIOHost (..)
  , IOHost
  
  , MonadReflex
  
  , module Reflex.Host.App.Switching
  
  
  ) where


import Data.Dependent.Sum

import Reflex.Class hiding (constant)
import Reflex.Host.Class

import Reflex.Host.App.Switching

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer.Class

import Prelude

type MonadReflex t m = (Reflex t, MonadHold t m, MonadFix m)
type IOHost t m = (ReflexHost t, MonadReflexCreateTrigger t m, MonadIO m, MonadIO (HostFrame t))

  
class (MonadWriter r (m r), MonadWriter s (m s)) => MapWriter m s r  where  
  
  -- | Embed one MonadAppWriter in another, a function is used to split the 
  --   result of the inner writer into parts to 'tell' the outer writer
  --   and a part to return.
  mapWriter :: (s -> (r, b)) -> m s a -> m r (a, b) 
  

  
instance MonadAppHost t r m => MonadAppHost t r (ReaderT e m) where
  
  performHost e = do
    env   <- ask
    lift . performHost $  (flip runReaderT env <$> e)

  collect m = do 
    env   <- ask
    lift (collect (runReaderT m env))

  
class (MonadReflex t m,  MonadWriter r m, SwitchMerge t r) => MonadAppHost t r m | m -> t r where
    
  -- | Run a monadic action during or immediately each frame in which the event fires, 
  -- and return the result in an event fired before other events.
  -- (Either in the same frame, or in the next immediate frame.
  performHost :: Event t (m a) -> m (Event t (a, r))
  
  
  -- | For the purposes of more efficient implementation.
  -- can be simply implemented in terms of MonadWriter, but more useful
  -- in this form.
  collect :: m a -> m (a, r)
  collect m = pass $ do
    a <- listen m
    return (a, const mempty)
  


class (ReflexHost t, MonadAppHost t r m, MonadIO m, MonadIO (HostFrame t), 
      MonadReflexCreateTrigger t m) => MonadIOHost t r m | m -> t r  where
        
  -- | Return a function to post events via IO to a fifo Event queue.
  askPostAsync :: m (HostFrame t [DSum (EventTrigger t)] -> IO ())      

  -- | Run a monadic HostFrame action for the purposes of it's IO effects. 
  -- e.g. for setting properties of an underlying UI or executing an XHR request.
  -- return the result in an Event in the frame immediately following
  performEvent :: Event t (HostFrame t a) -> m (Event t a)
  
  -- | performEvent run just for it's the effects without returning a value
  performEvent_ :: Event t (HostFrame t ()) -> m ()
  
  -- | Run host action for it's effects after construction, 
  -- return the result in an Event.
  schedulePostBuild :: HostFrame t a -> m (Event t a)
  
  -- | Run host action for it's effects after construction.
  schedulePostBuild_ :: HostFrame t () -> m ()  
  

