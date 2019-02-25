module Beeraffe where

import Affjax as Affjax
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Beeraffe.Array (shuffle)
import Beeraffe.Assets as Assets
import Beeraffe.Image as Image
import Beeraffe.Linear.V2 (mkV2)
import Beeraffe.Sprite (Sprite)
import Beeraffe.Sprite as Sprite
import Beeraffe.UI.Card as Card
import Beeraffe.UI.Game as Game
import Beeraffe.Color.RGBA as RGBA
import Data.Array ((..))
import Data.Array as Array
import Data.Either (Either (..))
import Data.String as String
import Data.String.Pattern as String.Pattern
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff.Compat (fromEffectFnAff)
import Effect.Exception as Exception
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)
import Prelude
import Web.DOM.ChildNode as DOM.ChildNode
import Web.DOM.Element as DOM.Element
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement as HTMLElement

main :: Effect Unit
main = HA.runHalogenAff do
    H.liftEffect $ Assets.loadCSS Assets.css
    sheet <- H.liftAff $ fromEffectFnAff $ Image.load Assets.sprites
    rsp <- H.liftAff $ Affjax.get Affjax.ResponseFormat.string Assets.words
    wordsTxt <- H.liftEffect $ case rsp.body of
        Left rfe -> Exception.throw $ Affjax.printResponseFormatError rfe
        Right w  -> pure w

    let sprites = Sprite.fromImageSheet 32 32 sheet
        words = map
            String.toUpper
            (String.split (String.Pattern.Pattern "\n") wordsTxt)

        templates = Array.zipWith
            (\sprite word ->
                { sprite: sprite
                , name: word
                , selected: false
                })
            sprites
            words

    shuffled <- H.liftEffect $ shuffle templates
    let initialGameState = Game.makeInitialState shuffled
    body <- HA.awaitBody

    H.liftEffect $ decorateBackground shuffled

    -- Hiding the loading indicator after the `initialGameState` has been
    -- constructed.
    mbLoading <- H.liftAff $ HA.selectElement (QuerySelector ".loading")
    for_ mbLoading $ \el ->
        H.liftEffect $
        DOM.ChildNode.remove $
        DOM.Element.toChildNode $
        HTMLElement.toElement $ el

    runUI (Game.component initialGameState) unit body

decorativeSprite :: Int -> RGBA.RGBA -> RGBA.RGBA -> Array Card.State -> Sprite
decorativeSprite size fg bg templates =
    let sprite0 = (unsafePartial $ Array.unsafeIndex templates 0).sprite
        width   = sprite0.width
        height  = sprite0.height
        offsets = do
            x <- 0 .. (size - 1)
            y <- 0 .. (size - 1)
            let i = y * size + x
            pure
                { offset: mkV2 (x * width) (y * height)
                , sprite: (unsafePartial $ Array.unsafeIndex templates i).sprite
                }

        tiles = Sprite.create do
            ms <- Sprite.new (width * size) (height * size)
            for_ offsets $ \t ->
                Sprite.blit ms t.offset t.sprite
            pure ms in

    Sprite.map
        (\c -> if RGBA.transparent c then fg else bg)
        tiles

decorateBackground :: Array Card.State -> Effect Unit
decorateBackground templates = do
    let sprite0 = (unsafePartial $ Array.unsafeIndex templates 0).sprite
        size    = 4
        width   = sprite0.width
        height  = sprite0.height
        purple  = decorativeSprite
                    size
                    (RGBA.mkRGB 119 54 124)
                    (RGBA.mkRGB 99 49 104)
                    templates

        reddish = decorativeSprite
                    size
                    (RGBA.mkRGB 122 27 27)
                    (RGBA.mkRGB 101 19 19)
                    (Array.drop 16 templates)

    H.liftEffect $ Assets.loadCSS $
        "html {" <>
        "background-image: url(\"" <> Sprite.toDataURL reddish <> "\");" <>
        "background-size: " <> show (width * size * 4) <> "px " <>
        show (height * size * 4) <> "px;" <>
        "}"

    H.liftEffect $ Assets.loadCSS $
        "section {" <>
        "background-image: url(\"" <> Sprite.toDataURL purple <> "\");" <>
        "background-size: " <> show (width * size * 4) <> "px " <>
        show (height * size * 4) <> "px;" <>
        "}"
