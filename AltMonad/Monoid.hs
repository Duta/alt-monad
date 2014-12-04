{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module AltMonad.Monoid where

import AltMonad.Category
import AltMonad.Hask
import qualified Data.Monoid as Normal
import Prelude (curry)

class Monoid i p c m
    | m -> i p where
  mid   :: i `c` m
  mcomb :: (m `p` m) `c` m

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
