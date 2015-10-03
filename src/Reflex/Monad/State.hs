{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}


module Reflex.Monad.State
  ( StateT 
  , State
  , runStateT
  , evalStateT

  , runState
  , evalState

  ) where


import Reflex
import Reflex.Monad.Class 


import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict as S
import Control.Monad.Reader.Class

import Control.Lens

import Prelude


data Env t s = Env 
  { _env_state    :: !s
  , _env_current  :: !Behavior t s
  , _env_updated  :: !Event t s
  }

    
newtype StateT s m a = StateT (S.StateT (Behavior t s, Event t s, s) m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadFix)
  
  
deriving instance MonadReader e m => MonadReader e (StateT s m)
deriving instance MonadWriter w m => MonadWriter w (StateT s m)

deriving instance MonadSample t m => MonadSample t (StateT s m)
deriving instance MonadHold t m => MonadHold t (StateT s m)

type State s a = StateT s Identity a

  
instance  MonadState s (StateT s m)  where
  get = lift  get
  put = lift . put


getFresh :: (Monad m, Splitable s i) => StateT s m i
getFresh = StateT $ state freshId


getSplit :: (Monad m, Splitable s i) => StateT s m s
getSplit = StateT $ state splitState



runSplit ::  (Monad m, Splitable s i) =>  StateT s m a -> State s (m a)
runSplit m = evalStateT m <$> getSplit
  

runStateT :: Monad m => StateT s m a -> s -> m (a, s)
runStateT (StateT m) = runStateT m 

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT (StateT m) = evalStateT m 


-- runState :: State s a -> s -> (a, s)
-- runState m  = runIdentity . runStateT m 
-- 
-- evalState :: State s a -> s -> a
-- evalState m = runIdentity . evalStateT m


tup ::  ((a, b), s) -> (a, (b, s))
tup ((a, b), s)  = (a, (b, s))

unTup :: (a, (b, s)) -> ((a, b), s)
unTup (a, (b, s))  = ((a, b), s)


instance (MonadPerform t m, Splitable s i) => MonadPerform t (StateT s m) where
  type Performs (StateT s m) a = Performs m a

  collect m = do 
    s <- StateT get
    (p, s') <- fmap unTup $ lift  $ 
      collect $ tup <$> runStateT m s
      
    StateT (put s') 
    return p
        
    
  perform e = do 
    s <- getSplit
    rec
      (a, es) <-  split <$> (lift . perform $ 
        attachWith (flip runStateT) r e)
      r <- hold s es 
    
    return a


instance MonadSwitch t m => MonadSwitch t (ReaderT e m) where
  switchM = lift . switchM
  switchMapM = lift . switchMapM

  
--     
    