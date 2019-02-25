module Beeraffe.Compose.Sprites
    ( area
    , render
    , exhaustive
    , mipmapped
    ) where

import Beeraffe.Color.RGBA as RGBA
import Beeraffe.Linear.Box (mkBox)
import Beeraffe.Linear.Box as Box
import Beeraffe.Linear.V2 (V2, mkV2)
import Beeraffe.Linear.V2 as V2
import Beeraffe.Sprite (Sprite)
import Beeraffe.Sprite as Sprite
import Control.MonadZero (guard)
import Data.Foldable (foldl)
import Data.Int (odd, toNumber)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Traversable (for_)
import Prelude

-- | Compute the number of non-transparent pixels in a sprite.
area :: Sprite -> Int
area sprite = Sprite.foldl
    (\acc _xy rgba -> if RGBA.transparent rgba then acc else acc + 1)
    0
    sprite

-- | A composition is basically just two sprites.  The first one is located at
-- the origin; and the other is at a given offset.
type Composition =
    { bigSprite      :: Sprite

    , smallSprite    :: Sprite
    , smallOffset    :: V2 Int
    , smallTransform :: Sprite.Transformations

    -- Note that the `smallestArea` is not necessarily the area of the small
    -- sprite.  The small sprite is the one that has the smallest `w*h`.  The
    -- big one may have an equal (or larger) `w*h`, and much more transparent
    -- pixels.
    --
    -- TODO(jaspervd): Since we now need to compute this for the big sprite as
    -- well, I think it makes sense to just store it in `sprite`.
    , smallestArea :: Int
    }

render :: Composition -> Sprite
render comp = Sprite.create do
    -- Create a canvas big enough to hold both sprites.
    ms <- Sprite.new (Box.width canvas) (Box.height canvas)

    -- Draw sprite A and B.
    Sprite.blit ms (- canvas.topLeft) comp.bigSprite
    Sprite.blit ms (comp.smallOffset - canvas.topLeft) comp.smallSprite

    -- We put sprite B on top of sprite A, so now we want to re-draw some pixels
    -- from sprite A to do the blend.
    for_ mbIntersection $ \intersection -> Box.for_ intersection $ \xy -> do
        when (odd $ V2.x xy + V2.y xy) $
            let pix = Sprite.index xy comp.bigSprite in
            unless (RGBA.transparent pix) $
                Sprite.write ms (xy - canvas.topLeft) pix

    pure ms
  where
    canvas         = Box.append bigBox smallBox
    mbIntersection = Box.intersection bigBox smallBox
    bigBox         = Box.fromDimensions comp.bigSprite
    smallBox       = Box.move comp.smallOffset $
                        Box.fromDimensions comp.smallSprite

data BestComposition
    = NoBestComposition
    | BestComposition
        { composition :: Composition
        , overlap     :: {area :: Int, distance :: Number}
        , score       :: Number
        }

instance bestCompositionSemigroup :: Semigroup BestComposition where
    append NoBestComposition y = y
    append x NoBestComposition = x
    append (BestComposition x) (BestComposition y) =
        BestComposition $ if x.score > y.score then x else y

score :: Composition -> BestComposition
score comp = fromMaybe NoBestComposition $ do
    let bigBox = Box.fromDimensions comp.bigSprite
        smallBox = Box.move comp.smallOffset (Box.fromDimensions comp.smallSprite)

    box <- Box.intersection bigBox smallBox
    let overlap = Box.foldl
            (\s xy ->
                let rgbaA = Sprite.index xy comp.bigSprite
                    rgbaB = Sprite.index (xy - comp.smallOffset) comp.smallSprite in
                if RGBA.transparent rgbaA || RGBA.transparent rgbaB
                    then s
                    else { area: s.area + 1
                         , distance: s.distance + RGBA.distance rgbaA rgbaB
                         })
            {area: 0, distance: 0.0}
            box

        cover = toNumber overlap.area / toNumber comp.smallestArea
    guard $ not $ overlap.area <= 0 || cover < 0.33 || cover > 0.75
    pure $ BestComposition
        { composition: comp
        , overlap: overlap
        , score: 1.0 - (overlap.distance / toNumber overlap.area)
        }

-- | Compare two sprites by size.
compareSizes
    :: Sprite -> Sprite -> {small :: Sprite, big :: Sprite}
compareSizes sa sb
    | Box.area (Box.fromDimensions sa) < Box.area (Box.fromDimensions sb) =
        {small: sa, big: sb}
    | otherwise =
        {small: sb, big: sa}


foldlExhaustive
    :: forall a. (a -> Composition -> a)
    -> a
    -> {small :: Sprite, big :: Sprite}
    -> a
foldlExhaustive f initial sprites = foldl
    (\acc0 transform ->
        -- This is a box holding all possible offsets for the smaller sprite.
        let small = Sprite.runTransformations transform sprites.small
            half = mkV2 (small.width `div` 2) (small.height `div` 2)
            offsetsBox = Box.move (-half) (Box.fromDimensions sprites.big) in
        Box.foldl
        -- Fold through all offsets in the offsetsBox, determining the best one.
        (\acc1 offset ->
            f acc1
                { bigSprite:      sprites.big
                , smallSprite:    small
                , smallOffset:    offset
                , smallestArea:   smallestArea
                , smallTransform: transform
                })
        acc0
        offsetsBox)
    initial
    Sprite.sizePreservingTransformations
  where
    -- We need to consider all transformations of the smaller sprite.  The area
    -- stays the same though.
    smallestArea = min (area sprites.small) (area sprites.big)

-- | Exhaustively find the best composition.
exhaustive' :: {small :: Sprite, big :: Sprite} -> Composition
exhaustive' sprites = case best of
    NoBestComposition ->
        -- NOTE(jaspervdj): This should not happen but it's probably better to
        -- return a stupid composition anyway.
        { bigSprite:      sprites.big
        , smallSprite:    sprites.small
        , smallOffset:    zero
        , smallTransform: []
        , smallestArea:   0
        }
    BestComposition c -> c.composition
  where
    -- Fold through all offsets in the offsetsBox, determining the best one.
    best = foldlExhaustive
        (\acc comp -> acc <> score comp)
        NoBestComposition
        sprites

exhaustive :: Sprite -> Sprite -> Composition
exhaustive sa sb = exhaustive' (compareSizes sa sb)

mipmapped :: Sprite -> Sprite -> Composition
mipmapped sa sb = foldl
    (\prevComp level ->
        let x   = V2.x prevComp.smallOffset * 2
            y   = V2.y prevComp.smallOffset * 2
            xy  = mkV2 x y
            box = mkBox (xy - mkV2 1 1) `Box.append` mkBox (xy + mkV2 3 3)

            small        = Sprite.runTransformations
                           prevComp.smallTransform level.small
            smallestArea = min (area level.small) (area level.big)
            bestComp     = Box.foldl
                (\best offset ->
                    let scored = score
                            { bigSprite:      level.big
                            , smallSprite:    small
                            , smallOffset:    offset
                            , smallTransform: prevComp.smallTransform
                            , smallestArea:   smallestArea
                            } in
                    scored <> best)
                NoBestComposition
                box in
        case bestComp of
            BestComposition c -> c.composition
            NoBestComposition ->
                { bigSprite:      level.big
                , smallSprite:    small
                , smallOffset:    mkV2 0 0
                , smallTransform: prevComp.smallTransform
                , smallestArea:   smallestArea
                })
    comp0
    (NonEmpty.tail samples)
  where
    -- Determine the longest edge of the two sprites.
    sprites = compareSizes sa sb
    samples = mipmap sprites

    level0 = NonEmpty.head samples
    comp0  = exhaustive' level0

mipmap
    :: {small :: Sprite, big :: Sprite}
    -> NonEmptyList {small :: Sprite, big :: Sprite}
mipmap full = go (NonEmpty.singleton full)
  where
    maxLongEdge = 16

    go levels =
        let level = NonEmpty.head levels in
        if max level.big.width level.big.height <= maxLongEdge
            then levels
            else go $ NonEmpty.cons
                        { small: Sprite.scaleQuarter level.small
                        , big:   Sprite.scaleQuarter level.big
                        }
                        levels
