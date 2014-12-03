{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module AltMonad.Category where

import AltMonad.Hask

class Category cat where
  id  :: cat a a
  (.) :: b `cat` c -> a `cat` b -> a `cat` c

instance Category Hask where
  id    = \x -> x
  g . f = \x -> g (f x)