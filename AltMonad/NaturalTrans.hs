{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module AltMonad.NaturalTrans where

import AltMonad.Functor

newtype a ~> b = NatTrans
  { transform :: (Functor c d a, Functor c d b)
              => a x -> b x
  }
