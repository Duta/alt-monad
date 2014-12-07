{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module AltMonad.Monad where

import AltMonad.Compose
import AltMonad.Functor
import AltMonad.Identity
import AltMonad.Monoid
import AltMonad.NaturalTrans

class    (Endofunctor c m, Monoid I (~.) (~>) m) => Monad c m
instance (Endofunctor c m, Monoid I (~.) (~>) m) => Monad c m
