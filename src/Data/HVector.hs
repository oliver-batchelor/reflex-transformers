{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, 
UndecidableInstances, DataKinds, KindSignatures, TypeOperators, GADTs, ConstraintKinds,
GeneralizedNewtypeDeriving, TypeFamilies #-}

module Data.HVector where

import Control.Lens hiding (only)

import Data.Type.List

import Control.Monad.Writer.Class
import Control.Monad.State.Strict


type Unique t ts = ('False ~ (Find t ts))  
  
type Member t ts = ('True ~ Find t ts)
  

data HVector (ts :: [*]) where
  Empty  :: HVector '[]
  (:>) ::  !t -> !(HVector ts) -> HVector (t ': ts)

  
element :: Member t ts => Lens' (HSet ts) t where
 
  
infixr 5 :>
  
instance Monoid (HSet '[]) where
  mappend Empty Empty = Empty
  mempty = Empty
  
instance (Unique t ts, Monoid t, Monoid (HSet ts)) => Monoid (HSet (t ': ts)) where
  mempty = mempty :> mempty
  mappend (a :> as) (b :> bs) = (a `mappend` b) :> (as `mappend` bs) 
  
  
