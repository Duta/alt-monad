{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module AltMonad.Monad where

import AltMonad.Category
import AltMonad.Compose
import AltMonad.Functor
import AltMonad.Identity
import AltMonad.NaturalTrans
import qualified Control.Monad as Normal

class HaskellFunctor m => Monad m where
  return :: I ~> m
  join :: (m ~. m) ~> m

returnDefault :: Normal.Monad m => I ~> m
returnDefault = NatTrans (Normal.return . runId)

joinDefault :: Normal.Monad m => (m ~. m) ~> m
joinDefault = NatTrans (Normal.join . runComp)

return' :: Monad m => a -> m a
return' = transform return . Id

join' :: Monad m => m (m a) -> m a
join' = transform join . Comp

(>>=) :: Monad m => m a -> (a -> m b) -> m b
x >>= f = join' (map f x)
