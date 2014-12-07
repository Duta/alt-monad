{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module AltMonad.Maybe where

import AltMonad
import AltMonad.HaskellMonoid
import Data.Function (const)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)

instance Functor Hask Hask Maybe where
  map f Nothing  = Nothing
  map f (Just x) = Just (f x)

instance Monoid I (~.) (~>) Maybe where
  mid   = NatTrans (Just  . runId)
  mcomb = NatTrans (join' . runComp)
   where
    join' Nothing  = Nothing
    join' (Just x) = x

instance HaskellMonoid m
      => Monoid () (,) Hask (Maybe m) where
  mid = const (Just empty)
  mcomb (Nothing, Nothing) = Nothing
  mcomb ( Just x, Nothing) = Just x
  mcomb (Nothing,  Just y) = Just y
  mcomb ( Just x,  Just y) = Just (x <> y)
