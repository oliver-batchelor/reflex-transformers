{-# LANGUAGE ConstraintKinds #-}    
    
module Reflex.Monad.IO.Class 
  ( module Reflex.Monad.Class
  , MonadAppHost (..)
  , MonadReflexIO
  
  ) where


import Reflex.Monad.Class
import Reflex.Host.Class

import Data.Dependent.Sum

import Control.Monad.IO.Class


type MonadReflexIO t m = (ReflexHost t, MonadReflexCreateTrigger t m, MonadIO m, MonadIO (HostFrame t))
    

class (MonadSwitch t m, MonadReflexIO t m) => MonadAppHost t m | m -> t  where
        
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