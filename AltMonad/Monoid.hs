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
import qualified Data.Monoid as Normal
import Prelude (curry)

class Category c
   => Monoid i p c m
    | m -> i p where
  mid   :: i `c` m
  mcomb :: (m `p` m) `c` m

class Monoid () (,) (->) m
   => HaskellMonoid m where
  empty :: m
  (<>)  :: m -> m -> m

instance Monoid () (,) (->) m
      => HaskellMonoid m where
  empty = mid ()
  (<>)  = curry mcomb

instance HaskellMonoid m
      => Normal.Monoid m where
  mempty  = empty
  mappend = (<>)
