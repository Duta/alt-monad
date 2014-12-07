{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
module AltMonad.Monoid where

class Monoid i p c m
    | m -> i p where
  mid   :: i `c` m
  mcomb :: (m `p` m) `c` m
