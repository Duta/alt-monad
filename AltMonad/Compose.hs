{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module AltMonad.Compose where

import AltMonad.Category
import AltMonad.Functor
import AltMonad.Hask

newtype (g ~. f) a = Comp { runComp :: f (g a) }

instance (Functor Hask Hask g, Functor Hask Hask f)
      => Functor Hask Hask (g ~. f) where
  map f = Comp . map (\x -> map f x) . runComp
