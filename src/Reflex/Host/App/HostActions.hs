module HostActions where


type HostAction t = HostFrame t (DList (DSum (EventTrigger t)))
type ApHostAction t = Ap (HostFrame t) (DList (DSum (EventTrigger t)))


data HostActions t = HostActions 
  { hostPerform   :: Events t (ApHostAction t) 
  , hostPostBuild :: ApHostAction t
  }  

instance ReflexHost t => Monoid (HostActions t) where
  mempty = HostActions mempty mempty
  mappend (HostActions p t) (HostActions p' t') = HostActions (mappend p p') (mappend t t')


instance ReflexHost t => Switchable t (HostActions t) where
    genericSwitch (HostActions perform postBuild) updated = do
      updatedPerform <- genericSwitch perform (hostPerform <$> updated)
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


class (ReflexHost t, MonadIO m, MonadIO (HostFrame t), MonadFix (HostFrame t), 
       MonadReflexCreateTrigger t m) => HostHasIO t m | m -> t       
  
class (HostHasIO t m) => HasPostAsync t m | m -> t where
  askPostAsync :: m (AppInputs t -> IO ())
   
  
class HasHostActions t r | r -> t where
  fromActions :: HostActions t -> r
 
instance HasHostActions t (HostActions t) where
  {-# INLINE fromActions #-}
  fromActions = id
  
  
{-# INLINE tellActions #-}
tellActions :: (ReflexHost t, HostWriter r m, HasHostActions t r) => HostActions t -> m ()
tellActions = tellHost . fromActions

{-# INLINE performEvent_ #-}
performEvent_ :: (ReflexHost t, HostWriter r m, HasHostActions t r) =>  Event t (HostFrame t ()) -> m ()
performEvent_  = tellActions . makePerform_

{-# INLINE generatePostBuild #-}
generatePostBuild :: (ReflexHost t, HostWriter r m, HasHostActions t r) => HostFrame t (DList (DSum (EventTrigger t))) -> m ()  
generatePostBuild = tellActions . makePostBuild

{-# INLINE schedulePostBuild #-}
schedulePostBuild :: (ReflexHost t, HostWriter r m, HasHostActions t r) => HostFrame t () -> m ()
schedulePostBuild action = generatePostBuild (action >> pure mempty)


  
{-# INLINE performEvent #-}
performEvent :: (HostHasIO t m, HostWriter r m, HasHostActions t r) =>  Event t (HostFrame t a) -> m (Event t a)
performEvent e = do 
  (event, construct) <- newEventWithConstructor
  tellActions . makePerform $ (\h -> h >>= liftIO . construct) <$> e
  return event
  

  
 
class (HasPostAsync t m, HasHostActions t r, MonadAppHost t r m) => MonadIOHost t r m | m -> t r


