-- | Simple 2-dimensional vectors.
module Beeraffe.Linear.V2
    ( V2 (..)
    , mkV2
    , x
    , y
    ) where

import Prelude

newtype V2 a = V2 {x :: a, y :: a}

instance showV2 :: Show a => Show (V2 a) where
    show (V2 r) = show r

instance eqV2 :: Eq a => Eq (V2 a) where
    eq (V2 l) (V2 r) = eq l r

instance semiringV2 :: Semiring a => Semiring (V2 a) where
    zero = V2 {x: zero, y: zero}
    add (V2 l) (V2 r) = V2 {x: add l.x r.x, y: add l.y r.y}

    -- The following implementations are doubtful at best.  Rather than use
    -- semigring I should consider having an AffineSpace typeclass.
    one = V2 {x: one, y: one}
    mul (V2 l) (V2 r) = V2 {x: mul l.x r.x, y: mul l.y r.y}

instance ringV2 :: Ring a => Ring (V2 a) where
    sub (V2 l) (V2 r) = V2 {x: sub l.x r.x, y: sub l.y r.y}

instance functorV2 :: Functor V2 where
    map f (V2 r) = V2 {x: f r.x, y: f r.y}

mkV2 :: forall a. a -> a -> V2 a
mkV2 x0 y0 = V2 {x: x0, y: y0}

x :: forall a. V2 a -> a
x (V2 v) = v.x

y :: forall a. V2 a -> a
y (V2 v) = v.y
