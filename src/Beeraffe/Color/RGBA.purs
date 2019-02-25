module Beeraffe.Color.RGBA
    ( RGBA (..)
    , mkRGBA
    , mkRGB
    , r, g, b, a
    , transparent
    , distance
    , average
    ) where

import Data.Array (unsafeIndex)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Int (toNumber, round)
import Math (sqrt)
import Partial.Unsafe (unsafePartial)
import Prelude

-- | We represent RGBA by an offset in an array.  This allows us to share the
-- representation with ImageData arrays.
data RGBA = RGBA Int (Array Int)

instance showRGBA :: Show RGBA where
    show c = "rgba" <> show [r c, g c, b c, a c]

mkRGBA :: Int -> Int -> Int -> Int -> RGBA
mkRGBA r0 g0 b0 a0 = RGBA 0 [r0, g0, b0, a0]

mkRGB :: Int -> Int -> Int -> RGBA
mkRGB r0 g0 b0 = mkRGBA r0 g0 b0 255

r :: RGBA -> Int
r (RGBA offset arr) = unsafePartial $ unsafeIndex arr offset

g :: RGBA -> Int
g (RGBA offset arr) = unsafePartial $ unsafeIndex arr (offset + 1)

b :: RGBA -> Int
b (RGBA offset arr) = unsafePartial $ unsafeIndex arr (offset + 2)

a :: RGBA -> Int
a (RGBA offset arr) = unsafePartial $ unsafeIndex arr (offset + 3)

transparent :: RGBA -> Boolean
transparent c = a c == 0

-- Assumes that alpha is 255.  Returns a number between 0 and 1.
distance :: RGBA -> RGBA -> Number
distance x y = dist / maximal
  where
    square :: Int -> Int
    square z = z * z

    dist = sqrt $
        2.0 * toNumber (square (r x - r y)) +
        4.0 * toNumber (square (g x - g y)) +
        3.0 * toNumber (square (b x - b y))

    -- I hope constant folding does a good job here.
    maximal = sqrt $
        2.0 * 255.0 * 255.0 +
        4.0 * 255.0 * 255.0 +
        3.0 * 255.0 * 255.0

average :: Array RGBA -> RGBA
average arr =
    let len   = toNumber (Array.length arr)
        avg f = round $ toNumber (sum (map f arr)) / len in
    mkRGBA (avg r) (avg g) (avg b) (avg a)
