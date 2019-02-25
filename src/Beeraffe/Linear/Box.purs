-- | Simple 2-dimensional bounding boxes.
module Beeraffe.Linear.Box
    ( Box
    , mkBox
    , append
    , fromDimensions
    , width
    , height
    , area
    , intersects
    , intersection
    , move
    , foldl
    , for_
    ) where

import Beeraffe.Linear.V2 (V2, mkV2, x, y)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Rec.Class as Rec
import Data.Maybe (Maybe (..))
import Prelude

type Box a = {topLeft :: V2 a, bottomRight :: V2 a}

mkBox :: forall a. V2 a -> Box a
mkBox v2 = {topLeft: v2, bottomRight: v2}

append :: forall a. Ord a => Box a -> Box a -> Box a
append l r =
    let tlx = min (x l.topLeft) (x r.topLeft)
        tly = min (y l.topLeft) (y r.topLeft)
        brx = max (x l.bottomRight) (x r.bottomRight)
        bry = max (y l.bottomRight) (y r.bottomRight) in
    {topLeft: mkV2 tlx tly, bottomRight: mkV2 brx bry}

-- | Create a box for anything that has width and height.
fromDimensions
    :: forall r. {width :: Int, height :: Int | r} -> Box Int
fromDimensions r =
    { topLeft: zero
    , bottomRight: mkV2 (r.width - 1) (r.height - 1)
    }

width :: Box Int -> Int
width b = x b.bottomRight - x b.topLeft + 1

height :: Box Int -> Int
height b = y b.bottomRight - y b.topLeft + 1

area :: Box Int -> Int
area b = width b * height b

-- | Check if there is an intersection in between two boxes.
intersects :: forall a. Ord a => Box a -> Box a -> Boolean
intersects l r = not $
    x l.bottomRight < x r.topLeft ||
    x l.topLeft > x r.bottomRight ||
    y l.bottomRight < y r.topLeft ||
    y l.topLeft > y r.bottomRight

-- | Like 'intersects', but actually obtains the intersection between two boxes.
intersection :: forall a. Ord a => Box a -> Box a -> Maybe (Box a)
intersection l r =
    let tlx = max (x l.topLeft) (x r.topLeft)
        tly = max (y l.topLeft) (y r.topLeft)
        brx = min (x l.bottomRight) (x r.bottomRight)
        bry = min (y l.bottomRight) (y r.bottomRight) in
    if tlx > brx || tly > bry
        then Nothing
        else Just {topLeft: mkV2 tlx tly, bottomRight: mkV2 brx bry}

-- | Move a box by a given offset.
move :: forall a. Semiring a => V2 a -> Box a -> Box a
move offset r =
    {topLeft: r.topLeft + offset, bottomRight: r.bottomRight + offset}

-- | Fold over all positions in a box.
foldl
    :: forall a. (a -> V2 Int -> a) -> a -> Box Int -> a
foldl f initial b = go (x b.topLeft) (y b.topLeft) initial
  where
    go x0 y0 acc0
        | x0 > x b.bottomRight = go (x b.topLeft) (y0 + 1) acc0
        | y0 > y b.bottomRight = acc0
        | otherwise            = go (x0 + 1) y0 (f acc0 (mkV2 x0 y0))

for_ :: forall a m. MonadRec m => Box Int -> (V2 Int -> m a) -> m Unit
for_ b f =
    Rec.tailRecM2 step (x b.topLeft) (y b.topLeft)
  where
    step x0 y0
        | x0 > x b.bottomRight = pure $ Rec.Loop {a: x b.topLeft, b: y0 + 1}
        | y0 > y b.bottomRight = pure $ Rec.Done unit
        | otherwise            = do
            _ <- f (mkV2 x0 y0)
            pure $ Rec.Loop {a: x0 + 1, b: y0}
