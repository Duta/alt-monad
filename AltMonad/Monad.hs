{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module AltMonad.Monad where

import AltMonad.Category
import AltMonad.Compose
import AltMonad.Functor
import AltMonad.Identity
import AltMonad.Monoid
import AltMonad.NaturalTrans
import qualified Control.Monad as Normal

class    (HaskellFunctor m, Monoid I (~.) (~>) m) => Monad m
instance (HaskellFunctor m, Monoid I (~.) (~>) m) => Monad m

class Monad m
   => HaskellMonad m where
  monadic :: a -> m a
  (>>=)   :: m a -> (a -> m b) -> m b

instance Monad m
      => HaskellMonad m where
  monadic =  transform mid   . Id
  x >>= f = (transform mcomb . Comp) (map f x)

instance HaskellMonad m
      => Normal.Monad m where
  return = monadic
  (>>=)  = (>>=)

midDefault :: Normal.Monad m => I ~> m
midDefault = NatTrans (Normal.return . runId)

mcombDefault :: Normal.Monad m => (m ~. m) ~> m
mcombDefault = NatTrans (Normal.join . runComp)
