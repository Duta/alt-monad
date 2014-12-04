{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PolyKinds #-}
module AltMonad.Category where

import AltMonad.Hask
import GHC.Exts (Constraint)

class Category cat where
  type Object cat o :: Constraint
  type Object cat o = ()
  id  :: Object cat a
      => a `cat` a
  (.) :: (Object cat a, Object cat b, Object cat c)
      => b `cat` c -> a `cat` b -> a `cat` c

instance Category Hask where
  id    = \x -> x
  g . f = \x -> g (f x)
