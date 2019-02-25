module Demo1 where

import Beeraffe.Compose.Sprites as Compose
import Beeraffe.Debug as Debug
import Beeraffe.Image as Image
import Beeraffe.Linear.V2 (mkV2)
import Beeraffe.Sprite as Sprite
import Data.Array as Array
import Data.Either (either)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Compat (fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Partial.Unsafe (unsafePartial)
import Prelude

main :: Effect Unit
main = Aff.runAff_ (either throwException pure)  do
    sheet <- fromEffectFnAff $ Image.load "./sprites.png"

    let images  = Sprite.fromImageSheet 32 32 sheet
        beer    = unsafePartial $ Array.unsafeIndex images 0
        giraffe = unsafePartial $ Array.unsafeIndex images 1

    liftEffect $ Debug.appendParagraph $ "Beer and giraffe:"
    liftEffect $ Debug.appendImage $ Sprite.toImage beer
    liftEffect $ Debug.appendImage $ Sprite.toImage giraffe
    liftEffect $ Debug.appendImage $ Sprite.toImage $
        Sprite.scaleQuarter giraffe

    liftEffect $ Debug.appendParagraph $ "Transformations:"
    for_ Sprite.sizePreservingTransformations $ \transform ->
        liftEffect $ Debug.appendImage $ Sprite.toImage $
        Sprite.runTransformations transform beer

    let compositions =
            [ {smallSprite: beer, bigSprite: giraffe, smallOffset: mkV2 0 3, smallestArea: 1, smallTransform: []}
            , {smallSprite: beer, bigSprite: giraffe, smallOffset: mkV2 2 7, smallestArea: 1, smallTransform: []}
            , {smallSprite: beer, bigSprite: giraffe, smallOffset: mkV2 (-3) (-2), smallestArea: 1, smallTransform: []}
            ]

    liftEffect $ Debug.appendParagraph $ "Sample compositions:"
    for_ compositions $ \comp ->
        liftEffect $ Debug.appendImage $ Sprite.toImage $
            Compose.render comp

    best <- liftEffect $ Debug.appendBenchmark "exhaustive" $
        \_ -> Compose.exhaustive beer giraffe

    liftEffect $ Debug.appendParagraph $ "Best beeraffe:"
    liftEffect $ Debug.appendImage $ Sprite.toImage $
        Compose.render best
    best' <- liftEffect $ Debug.appendBenchmark "mipmapped" $
        \_ -> Compose.mipmapped beer giraffe
    liftEffect $ Debug.appendParagraph $ "Best' beeraffe:"
    liftEffect $ Debug.appendImage $ Sprite.toImage $
        Compose.render best'
