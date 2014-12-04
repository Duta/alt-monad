{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module AltMonad.Functor where

import AltMonad.Category
import AltMonad.Hask
import qualified Data.Functor as Normal

class (Category c, Category d)
   => Functor c d f
    | f -> c d where
  map :: (Object c a, Object c b, Object d (f a), Object d (f b))
      => a `c` b -> f a `d` f b

class    Functor a a f => Endofunctor a f
instance Functor a a f => Endofunctor a f

class    Endofunctor Hask f => HaskellFunctor f
instance Endofunctor Hask f => HaskellFunctor f

instance HaskellFunctor f
      => Normal.Functor f where
  fmap = map
