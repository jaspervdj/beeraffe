-- | Wraps image loading to do some operations we want to support.
module Beeraffe.Sprite
    ( Sprite
    , create
    , fromImage
    , fromImageSheet
    , toImage
    , toDataURL

    , foldl
    , map
    , index

    , MutableSprite
    , new
    , unsafeFreeze
    , write
    , blit

      -- Transformations
    , Transformation
    , Transformations
    , runTransformations
    , sizePreservingTransformations

    , rotateClockwise
    , rotateCounterClockwise
    , mirrorHorizontal
    , scaleQuarter
    ) where

import Beeraffe.Color.RGBA (RGBA (..))
import Beeraffe.Color.RGBA as RGBA
import Beeraffe.Image (Image)
import Beeraffe.Image as Image
import Beeraffe.Linear.V2 (V2, mkV2)
import Beeraffe.Linear.V2 as V2
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Rec.Class as Rec
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.MonadZero (guard)
import Data.Array ((..))
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Foldable as Foldable
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3, runFn5)
import Effect (Effect)
import Prelude

foreign import createPixelData       :: forall h. Fn2 Int Int (ST h (STArray h Int))
foreign import unsafeFreezePixelData :: forall h. STArray h Int -> ST h (Array Int)
foreign import toDataURL             :: Sprite -> String

type Sprite =
    { width  :: Int
    , height :: Int
    , pixels :: Array Int
    }

create :: (forall h. ST h (MutableSprite h)) -> Sprite
create mx = ST.run do
    x <- mx
    unsafeFreeze x

fromImage :: Image -> Sprite
fromImage i =
    { width: Image.width i
    , height: Image.height i
    , pixels: runFn5 Image.pixelData 0 0 (Image.width i) (Image.height i) i
    }

fromImageSheet :: Int -> Int -> Image -> Array Sprite
fromImageSheet tw th image = do
    y <- 0 .. (h - 1)
    x <- 0 .. (w - 1)
    let pixels = runFn5 Image.pixelData (x * tw) (y * th) tw th image
        sprite = {width: tw, height: th, pixels: pixels}
    guard $ not $ transparent sprite
    pure sprite
  where
    w = Image.width image `div` tw
    h = Image.height image `div` th

toImage :: Sprite -> Image
toImage s = runFn3 Image.fromPixelData s.width s.height s.pixels

foldl
    :: forall a.
       (a -> V2 Int -> RGBA -> a)
    -> a
    -> Sprite
    -> a
foldl f acc0 sprite = go 0 0 acc0
  where
    go x y acc
        | x >= sprite.width  = go 0 (y + 1) acc
        | y >= sprite.height = acc
        | otherwise          =
            let offset = (y * sprite.width + x) * 4
                rgba   = RGBA offset sprite.pixels in
            go (x + 1) y (f acc (mkV2 x y) rgba)

for_
    :: forall a m. MonadRec m
    => Sprite
    -> (V2 Int -> RGBA -> m a)
    -> m Unit
for_ sprite f =
    Rec.tailRecM2 step 0 0
  where
    step x y
        | x >= sprite.width  = pure $ Rec.Loop {a: 0, b: y + 1}
        | y >= sprite.height = pure $ Rec.Done unit
        | otherwise          = do
            let offset = (y * sprite.width + x) * 4
                rgba   = RGBA offset sprite.pixels
            _ <- f (mkV2 x y) rgba
            pure $ Rec.Loop {a: x + 1, b: y}

map :: (RGBA -> RGBA) -> Sprite -> Sprite
map f s = create do
    ms <- new s.width s.height
    for_ s $ \xy pix -> write ms xy (f pix)
    pure ms

index :: V2 Int -> Sprite -> RGBA
index v2 sprite =
    RGBA (((V2.y v2 * sprite.width) + (V2.x v2)) * 4) sprite.pixels

transparent :: Sprite -> Boolean
transparent = foldl (\acc _ pix -> acc && RGBA.transparent pix) true

type MutableSprite h =
    { width  :: Int
    , height :: Int
    , pixels :: STArray h Int
    }

new :: forall h. Int -> Int -> ST h (MutableSprite h)
new w h = do
    pixels <- runFn2 createPixelData w h
    pure {width: w, height: h, pixels: pixels}

unsafeFreeze :: forall h. MutableSprite h -> ST h Sprite
unsafeFreeze ms = do
    pixels <- unsafeFreezePixelData ms.pixels
    pure $ ms {pixels = pixels}

write :: forall h. MutableSprite h -> V2 Int -> RGBA -> ST h Unit
write ms xy pix = do
    let offset = ((V2.y xy * ms.width) + (V2.x xy)) * 4
    _ <- STArray.poke offset       (RGBA.r pix) ms.pixels
    _ <- STArray.poke (offset + 1) (RGBA.g pix) ms.pixels
    _ <- STArray.poke (offset + 2) (RGBA.b pix) ms.pixels
    _ <- STArray.poke (offset + 3) (RGBA.a pix) ms.pixels
    pure unit

-- | Copy a sprite onto a mutable sprite at a given offset. The sprite must fit
-- in that space; this is not checked.
blit :: forall h. MutableSprite h -> V2 Int -> Sprite -> ST h Unit
blit ms offset sprite = for_ sprite $ \xy pix ->
    unless (RGBA.transparent pix) $ write ms (offset + xy) pix

data Transformation
    = RotateClockwise
    | RotateCounterClockwise
    | Rotate180
    | MirrorHorizontal

type Transformations = Array Transformation

runTransformations :: Transformations -> Sprite -> Sprite
runTransformations ts sprite = Foldable.foldl
    (\s t -> case t of
        RotateClockwise        -> rotateClockwise s
        RotateCounterClockwise -> rotateCounterClockwise s
        Rotate180              -> rotate180 s
        MirrorHorizontal       -> mirrorHorizontal s)
    sprite
    ts

-- | All eight possible transformations of a sprite (rotate, flip...).
sizePreservingTransformations :: Array Transformations
sizePreservingTransformations =
    [ []
    , [RotateClockwise]
    , [RotateCounterClockwise]
    , [Rotate180]
    , [MirrorHorizontal]
    , [RotateClockwise, MirrorHorizontal]
    , [RotateCounterClockwise, MirrorHorizontal]
    , [Rotate180, MirrorHorizontal]
    ]

rotateClockwise :: Sprite -> Sprite
rotateClockwise sprite = create do
    ms <- new sprite.height sprite.width
    for_ sprite $ \orig pix ->
        let x = sprite.height - V2.y orig - 1
            y = V2.x orig in
        write ms (mkV2 x y) pix
    pure ms

rotateCounterClockwise :: Sprite -> Sprite
rotateCounterClockwise sprite = create do
    ms <- new sprite.height sprite.width
    for_ sprite $ \orig pix ->
        let x = V2.y orig
            y = sprite.width - V2.x orig - 1 in
        write ms (mkV2 x y) pix
    pure ms

rotate180 :: Sprite -> Sprite
rotate180 sprite = create do
    ms <- new sprite.width sprite.height
    for_ sprite $ \orig pix ->
        let x = sprite.width - V2.x orig - 1
            y = sprite.height - V2.y orig - 1 in
        write ms (mkV2 x y) pix
    pure ms

mirrorHorizontal :: Sprite -> Sprite
mirrorHorizontal sprite = create do
    ms <- new sprite.width sprite.height
    for_ sprite $ \orig pix ->
        let x = sprite.width - V2.x orig - 1
            y = V2.y orig in
        write ms (mkV2 x y) pix
    pure ms

-- | Scale down to a quarter of the original size.  Very useful for mipmapping.
scaleQuarter :: Sprite -> Sprite
scaleQuarter sprite = create do
    let w = (sprite.width + 1) `div` 2
        h = (sprite.width + 1) `div` 2
    ms <- new w h
    let step x y
            | x >= w    = pure $ Rec.Loop {a: 0, b: y + 1}
            | y >= h    = pure $ Rec.Done unit
            | otherwise = do
                let pixels = do
                        x' <- [x * 2, x * 2 + 1]
                        y' <- [y * 2, y * 2 + 1]
                        guard $ x' <= sprite.width && x' <= sprite.height
                        pure $ index (mkV2 x' y') sprite

                write ms (mkV2 x y) $ RGBA.average pixels
                pure $ Rec.Loop {a: x + 1, b: y}

    Rec.tailRecM2 step 0 0
    pure ms
