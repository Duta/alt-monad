{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module AltMonad.NaturalTrans where

import AltMonad.Functor

newtype a ~> b = NatTrans
  { transform :: (HaskellFunctor a, HaskellFunctor b)
              => a x -> b x
  }
