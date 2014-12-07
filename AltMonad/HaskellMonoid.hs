{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module AltMonad.HaskellMonoid where

import AltMonad.Hask
import AltMonad.Monoid
import qualified Data.Monoid as Normal
import Data.Tuple (curry)

class Monoid () (,) Hask m
   => HaskellMonoid m where
  empty :: m
  (<>)  :: m -> m -> m

instance Monoid () (,) Hask m
      => HaskellMonoid m where
  empty = mid ()
  (<>)  = curry mcomb

instance HaskellMonoid m
      => Normal.Monoid m where
  mempty  = empty
  mappend = (<>)
