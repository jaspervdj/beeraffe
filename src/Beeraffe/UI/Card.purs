module Beeraffe.UI.Card
    ( State
    , price
    , sell

    , Composition
    , compose

    , Query (..)
    , Message (..)
    , component
    ) where

import Beeraffe.Compose as Compose
import Beeraffe.Sprite as Sprite
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude

type State =
    { sprite   :: Sprite.Sprite
    , name     :: String
    , selected :: Boolean
    }

price :: State -> Int
price card = String.length card.name

sell :: State -> Int
sell card = price card - 1

type Composition =
    { left   :: String
    , right  :: String
    , parts  :: {l :: String, m :: String, r :: String}
    , name   :: String
    , sprite :: Unit -> Sprite.Sprite
    , coins  :: Int
    }

compose
    :: State -> State -> Maybe Composition
compose x y = case Compose.composeWords x.name y.name of
    Nothing -> Nothing
    Just w  -> Just
        { left:   w.left
        , right:  w.right
        , parts:  w.parts
        , name:   w.result
        , sprite: \_ -> Compose.composeSprites x.sprite y.sprite
        , coins:  min (sell x) (sell y)
        }

data Query a
    -- | The card was selected.
    = Select a
    -- | The card was unselected.
    | Unselect a
    -- | Get the card state.
    | Get (State -> a)

data Message
    = Selected State

component
    :: State -> H.Component HH.HTML Query Unit Message Aff
component initialCard = H.component
    { initialState: const initialCard
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render card = HH.div
        [ HP.classes $
            [HH.ClassName "card"] <>
            (if card.selected then [HH.ClassName "selected"] else [])
        , HE.onClick $ HE.input \_ x -> Select x
        ]
        [ HH.div
            [HP.class_ $ HH.ClassName "card-contents"]
            (renderCardContents card)
        ]

    -- renderCardContents :: State -> H.ComponentHTML Query
    renderCardContents card =
        [ HH.p
            [HP.classes [HH.ClassName "handle", HH.ClassName "left"]]
            [HH.text $ String.take 3 card.name]
        , HH.p
            [HP.classes [HH.ClassName "handle", HH.ClassName "right"]]
            [HH.text $ String.drop (String.length card.name - 3) card.name]
        , HH.img
            [ HP.src (Sprite.toDataURL card.sprite)
            , HP.width (4 * card.sprite.width)
            , HP.height (4 * card.sprite.height)
            ]
        , HH.p [] [HH.text card.name]
        , HH.p [] [HH.text $ "$" <> show (price card)]
        ]

    eval :: Query ~> H.ComponentDSL State Query Message Aff
    eval = case _ of
        Select next -> do
            H.modify_ $ \s -> s {selected = true}
            state <- H.get
            H.raise $ Selected state
            pure next
        Unselect next -> do
            H.modify_ $ \s -> s {selected = false}
            pure next
        Get f -> do
            state <- H.get
            pure (f state)
