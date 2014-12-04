{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module AltMonad.Maybe where

import AltMonad
import Data.Maybe (Maybe(..))

instance Functor Hask Hask Maybe where
  map f Nothing  = Nothing
  map f (Just x) = Just (f x)

instance Monoid I (~.) (~>) Maybe where
  mid   = NatTrans (Just  . runId)
  mcomb = NatTrans (join' . runComp)
   where
    join' Nothing  = Nothing
    join' (Just x) = x
