{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module AltMonad.Identity where

import AltMonad.Category
import AltMonad.Functor
import AltMonad.Hask

newtype I a = Id { runId :: a }

instance Functor Hask Hask I where
  map f = Id . f . runId
