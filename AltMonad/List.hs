{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module AltMonad.List where

import AltMonad
import Data.Function (const)
import Data.List (concat, (++))
import Data.Tuple (uncurry)

instance Functor Hask Hask [] where
  map f []     = []
  map f (x:xs) = f x:map f xs

instance Monoid I (~.) (~>) [] where
  mid   = NatTrans ((:[])  . runId)
  mcomb = NatTrans (concat . runComp)

instance Monoid () (,) Hask [a] where
  mid   = const []
  mcomb = uncurry (++)
