module Beeraffe.UI.Game
    ( ShopId
    , DeckId
    , State
    , makeInitialState

    , Query (..)

    , component
    ) where

import Beeraffe.Array (insertAt, pick)
import Beeraffe.UI.Card as Card
import Beeraffe.UI.Notification as Notification
import Beeraffe.UI.Sorceress as Sorceress
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude

type ShopId = Int
type DeckId = Int

type State =
    { templates  :: Array Card.State
    , shop       :: Array {id :: ShopId, template :: Card.State}
    , deck       :: Array {id :: DeckId, template :: Card.State}
    , selection  :: Maybe {id :: DeckId, card :: Card.State}
    , nextId     :: Int
    , wealth     :: Int

    , sorceress  :: Sorceress.State
    }

data Query a
    = Pass a
    | HandleDeckCard DeckId Card.Message a
    | HandleShopCard ShopId Card.Message a
    | SellDeckCard DeckId Card.State a

type ChildQuery = Coproduct3 Card.Query Card.Query Notification.Query
type ChildSlot = Either3 ShopId DeckId Unit

makeInitialState
    :: Array Card.State -> State
makeInitialState templates =
    let shop = Array.mapWithIndex
                    (\i tpl -> {id: i, template: tpl})
                    (Array.take 3 templates)
        deck = Array.mapWithIndex
                    (\i tpl -> {id: i, template: tpl})
                    (Array.take 1 $ Array.drop 3 templates) in
    { templates: templates
    , shop: shop
    , deck: deck
    , selection: Nothing
    , nextId: 100
    , wealth: 60
    , sorceress: Sorceress.initialState
    }

composeSelection
    :: Card.State -> State
    -> Maybe {id :: DeckId, composition :: Card.Composition}
composeSelection card game = do
    selection <- game.selection
    comp <- Card.compose card selection.card
    pure {id: selection.id, composition: comp}

component
    :: State -> H.Component HH.HTML Query Unit Void Aff
component initialGameState = H.parentComponent
    { initialState: const initialGameState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
    render game = HH.div [] $
        -- Shop header
        [ HH.header []
            [ HH.p
                [HP.class_ $ HH.ClassName "left"]
                [HH.text "THE SHOP"]
            , HH.p
                [HP.class_ $ HH.ClassName "right"]
                [HH.text $ "WEALTH $" <> show game.wealth]
            , HH.p [] [HH.text "BEERAFFE"]
            ]
        , HH.section [] $
            -- Cards in shop
            (do
                inventory <- game.shop
                pure $ HH.slot'
                    CP.cp1
                    inventory.id
                    (Card.component inventory.template)
                    unit
                    (HE.input $ HandleShopCard inventory.id))
        -- Deck header
        , HH.header [] $
            [ HH.p
                [HP.class_ $ HH.ClassName "left"]
                [HH.text "YOUR THINGS"]
            ] <>
            -- Sell button
            (case game.selection of
                Nothing -> []
                Just sel -> Array.singleton $ HH.p
                    [ HP.classes [HH.ClassName "button", HH.ClassName "right"]
                    , HE.onClick $ HE.input \_ -> SellDeckCard sel.id sel.card
                    ]
                    [ HH.text $ "Sell for $" <> show (Card.sell sel.card) ]) <>
            -- NBSP for formatting; I deserve some sort of medal for this, and
            -- not a congratulatory one.
            [ HH.p [] [HH.text "\xA0"] ]
        -- Card in your deck.
        , HH.section
            [ HP.classes $
                [HH.ClassName "deck"] <>
                (case game.selection of
                    Nothing -> []
                    Just _  -> [HH.ClassName "selected"])
            ] $
            (do
                card <- game.deck
                pure $ HH.slot'
                    CP.cp2
                    card.id
                    (Card.component card.template)
                    unit
                    (HE.input $ HandleDeckCard card.id)) <>
            Sorceress.render game.sorceress
        , HH.slot' CP.cp3 unit Notification.component unit absurd
        , HH.footer []
            [ HH.text "BEERAFFE"
            , HH.sup [] [HH.text "beta"]
            , HH.text " by "
            , HH.a
                [HP.href "https://twitter.com/jaspervdj"]
                [HH.text "jaspervdj"]
            , HH.text ". Source available "
            , HH.a
                [HP.href "https://github.com/jaspervdj/beeraffe"]
                [HH.text "here"]
            , HH.text "."
            ]
        ]

    eval
        :: Query
        ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
    eval = case _ of
        Pass next -> pure next

        HandleDeckCard cardId (Card.Selected card) next -> do
            state <- H.get

            -- Start by unselecting the last card.
            for_ state.selection $ \sel ->
                H.query' CP.cp2 sel.id $ H.action Card.Unselect

            case composeSelection card state of
                Nothing
                    -- We clicked the same card again.  It should be unselected.
                    | Just cardId == map _.id state.selection ->
                        H.modify_ $ \s -> s {selection = Nothing}

                    -- We clicked another card.
                    | otherwise -> do
                        H.modify_ $ \s -> s
                            {selection = Just {id: cardId, card: card}}

                -- We created a composition.
                Just comp -> do
                    let insertIndex = fromMaybe 0 $ Array.findIndex
                            (\c -> c.id == cardId)
                            state.deck

                    -- Set selection to nothing, remove both cards, adding
                    -- coins.
                    H.modify_ $ \s -> s
                        { selection = Nothing
                        , deck = Array.filter
                            (\b -> b.id /= comp.id && b.id /= cardId) s.deck
                        , wealth = s.wealth + comp.composition.coins
                        }

                    -- Show a notification
                    let name = comp.composition.name
                    pushNotification
                        [ comp.composition.left <> " + " <> comp.composition.right
                        , "= " <> name
                        , "$" <> show comp.composition.coins <> " WEALTH GET!"
                        ]

                    -- Calculating sprite can take a bit.
                    sprite <- H.liftAff $ pure (comp.composition.sprite unit)

                    -- Add new card.
                    let newTpl =
                            { sprite:   sprite
                            , name:     name
                            , selected: false
                            }

                    H.modify_ $ \s -> s
                        { nextId = s.nextId + 1
                        , deck = insertAt
                            insertIndex
                            {id: s.nextId, template: newTpl}
                            s.deck
                        }

                    updateSorceress $ Sorceress.Compose comp.composition

            pure next

        HandleShopCard inventoryId (Card.Selected card) next -> do
            -- Start by unselecting the bought card.
            _ <- H.query' CP.cp1 inventoryId $ H.action Card.Unselect

            -- Get the money and remove the card from the shop.  Then add a new
            -- card to the shop.
            state0 <- H.get
            if (state0.wealth >= Card.price card)
                then do
                    supply <- H.liftEffect $ pick state0.templates
                    H.modify_ $ buyFromShop inventoryId card supply
                    pushNotification ["You bought " <> card.name <> "."]
                    updateSorceress $ Sorceress.Buy card
                else
                    -- Show a notification
                    pushNotification
                        [ "Cannot buy " <> card.name <> "..."
                        , "You have insufficient WEALTH."
                        ]

            pure next

        SellDeckCard deckId card next -> do
            state <- H.get
            for_ state.selection $ \sel ->
                H.query' CP.cp2 sel.id $ H.action Card.Unselect
            H.modify_ $ sellCardFromDeck deckId card
            pushNotification
                [ "Sold " <> card.name
                , "for $" <> show (Card.sell card) <> " WEALTH."
                ]
            updateSorceress $ Sorceress.Sell card
            pure next

    pushNotification lines = void $
        H.query' CP.cp3 unit $ H.action $ Notification.Push lines

    updateSorceress event = do
        state <- H.get
        sorc  <- Sorceress.update askSuggestion event state.sorceress
        H.modify_ $ \s -> s {sorceress = sorc}

    askSuggestion = do
        -- It's very slow since we need to get all cards.  Hopefully,
        -- the tutorial doesn't last long.  Quadratic, horrible!!
        cards <- Map.values <$> H.queryAll' CP.cp2 (H.request Card.Get)
        let matches = do
                x <- cards
                y <- cards
                guard $ x.name /= y.name
                List.fromFoldable $ Card.compose x y
        pure $ List.head matches

sellCardFromDeck
    :: DeckId      -- ^ ID of the card to sell
    -> Card.State  -- ^ Card to sell
    -> State -> State
sellCardFromDeck inventoryId sell game = game
    { selection = Nothing
    , deck      = Array.filter (\c -> c.id /= inventoryId) game.deck
    , wealth    = game.wealth + Card.sell sell
    }

buyFromShop
    :: ShopId      -- ^ ID of the bought card
    -> Card.State  -- ^ Bought card
    -> Card.State  -- ^ Replacement card for the shop
    -> State -> State
buyFromShop inventoryId bought supply game0 =
    let -- We'll try to insert the supply back at the same index.
        shopIdx = fromMaybe 0 $
            Array.findIndex (\c -> c.id == inventoryId) game0.shop

        -- Remove the card from the shop, and add it to the deck.
        game1 = game0
            { wealth = game0.wealth - Card.price bought
            , shop   = Array.filter (\c -> c.id /= inventoryId) game0.shop
            , nextId = game0.nextId + 1
            , deck   = Array.cons
                        {id: game0.nextId, template: bought {selected = false}}
                        game0.deck
            }

        -- Supply the shop
        game2 = game1
            { nextId = game1.nextId + 1
            , shop   = insertAt shopIdx
                        {id: game1.nextId, template: supply} game1.shop
            } in

    game2
