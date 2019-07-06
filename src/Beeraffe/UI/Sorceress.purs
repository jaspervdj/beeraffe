module Beeraffe.UI.Sorceress
    ( Dialog
    , State
    , initialState

    , Event (..)
    , Tutorial (..)
    , render
    , update
    ) where

import Beeraffe.Array (pick)
import Beeraffe.Assets as Assets
import Beeraffe.UI.Card as Card
import Data.Array as Array
import Data.Maybe (Maybe (..))
import Data.String as String
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random as Random
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude

type Dialog = Array String

type State =
    { tutorial     :: Tutorial
    , buys         :: Int
    , compositions :: Int
    , dialog       :: Dialog
    }

data Event
    = Buy Card.State
    | Compose Card.Composition
    | Sell Card.State

data Tutorial
    = NeedToBuyStuff Boolean
    | NeedToCompose Card.Composition Boolean
    | Epilogue
    | Done

initialState :: State
initialState =
    let tutorial0 = NeedToBuyStuff false in
    { tutorial:     tutorial0
    , buys:         0
    , compositions: 0
    , dialog:       tutorialDialog tutorial0
    }

render :: forall a b. State -> Array (HH.HTML a b)
render sorceress
    | Array.null sorceress.dialog = []
    | otherwise                   = Array.singleton $ HH.div
        [HP.class_ (HH.ClassName "tutorial")]
        [ HH.p
            [HP.class_ (HH.ClassName "title")]
            [HH.text $ "SORCERESS of THINGS and CONJURER of WEALTH"]
        , HH.div
            [HP.class_ (HH.ClassName "content")] $
            [HH.img [HP.src Assets.sorceress]] <>
            map (\l -> HH.p [] [HH.text l]) sorceress.dialog <>
            [HH.div [HP.class_ (HH.ClassName "clear")] []]
        ]

-- Check if we need to advance the tutorial, and do so.
update
    :: forall m. MonadEffect m
    => (m (Maybe Card.Composition))  -- ^ Obtain a suggestion
    -> Event
    -> State
    -> m State
update askSuggestion event state0 = do
    let state1 = case event of
            Buy     _ -> state0 {buys = state0.buys + 1}
            Compose _ -> state0 {compositions = state0.compositions + 1}
            _         -> state0

    case state1.tutorial of
        -- If we're done with the tutorial, we fall through to other dialog.
        Done -> do
            -- Most of the time, we don't want to react at all.
            n <- liftEffect $ Random.randomInt 1 $ case event of
                Buy     _ -> 9
                Compose _ -> 5
                Sell    _ -> 5
            dialog <- if n == 1 then genericDialog event else pure []
            pure state1 {dialog = dialog}

        -- After two composes, the user knows enough.
        _ | state1.compositions >= 3 -> pure $
            state1 {tutorial = Done, dialog = []}
        _ | state1.compositions >= 2 -> pure $
            state1 {tutorial = Epilogue, dialog = tutorialDialog Epilogue}

        -- If not, see if there are two cards in the deck that can be
        -- composed.
        _ -> do
            mbSuggestion <- askSuggestion
            let tut = case mbSuggestion of
                    Nothing -> NeedToBuyStuff (state1.compositions >= 1)
                    Just m  -> NeedToCompose m (state1.compositions >= 1)

            -- Write tutorial.
            pure $ state1 {tutorial = tut, dialog = tutorialDialog tut}

tutorialDialog :: Tutorial -> Dialog
tutorialDialog tutorial = case tutorial of
    Done -> []
    NeedToBuyStuff hasMerged
        | hasMerged ->
            [ "So, MINION, it looks like you don't have enough THINGS to do " <>
                "anything useful."
            , "Don't just stand around, add some more stuff to YOUR THINGS " <>
                "spending some of your laughable WEALTH in THE SHOP."
            ]
        | otherwise ->
            [ "Hello, MINION! Welcome to BEERAFFE."
            , "I must say I am impressed.  You look like you are even more " <>
                "stupid than the last MINION that passed by; and they ended " <>
                "up dying in a tragic but hilarious DARWINIAN ACCIDENT." -- CHECK
            , "Anyway, enough about you. Let's talk about me!"
            , "They call me the SORCERESS of THINGS and CONJURER of WEALTH " <>
                "but I have many more names.  Some people refer to me as " <>
                "WARLOCK of THE INTERNET and SUMMONER of LOL, but you can " <>
                "just call me BOSS.  No, actually, it would be " <>
                "better if you didn't address me at all and left me in " <>
                "peace as much as possible."
            , "So, just leave me alone for now and go spend some WEALTH in " <>
                "THE SHOP."
            ]
    NeedToCompose m reminder ->
        [if reminder
            then "I can tell from the stupid look on your already stupid " <>
                    "face that you didn't quite get it.  Let me repeat the " <>
                    "rules, but this will be the last time -- they aren't " <>
                    "paying me for this, you know!"
            else "Right, so here we go.  If you have two or more THINGS, " <>
                    "you may be able to compose them.  However, you can't " <>
                    "just compose anything, composing is a respected form " <>
                    "of PURPLE MAGIC and has very strict rules."
        , "One of the last three letters of the first THING must match " <>
            "one of the first three letters of the THING you are trying " <>
            "to compose it with."
        , "For example, you can currently compose " <> m.left <> " and " <>
            m.right <> ", because " <> m.parts.m <> " appears in the " <>
            "last three letters of " <> m.left <> " as well as in " <>
            "the first three letters of " <> m.right <> "."
        , "We get: " <> m.parts.l <> " + " <> m.parts.m <> " + " <> m.parts.r
        , "Click " <> m.left <> ", then click " <> m.right <>
            " to compose them."
        ]
    Epilogue ->
        [ "Well, MINION, that was only a mediocre job at best.  But I guess " <>
            "if we consider your lack of great THINGS to compose, " <>
            "and your general lack of intelligence, you didn't do too bad."
        , "By composing THINGS you increase your WEALTH, so you " <>
            "can buy new THINGS in THE SHOP."
        , "This will allow you to create and compose more impressive THINGS."
        ]

an :: String -> String
an name = case String.toLower (String.take 1 name) of
    "a" -> "an " <> name
    "e" -> "an " <> name
    "i" -> "an " <> name
    "o" -> "an " <> name
    "u" -> "an " <> name
    _   -> "a "  <> name

genericDialog :: forall m. MonadEffect m => Event -> m Dialog
genericDialog (Buy card) = liftEffect $ pick
    [ [ "Wow, you got ripped off MINION!  Everyone knows the street value " <>
        "of " <> an card.name <> " is only $" <> show (Card.price card - 3) <>
        " WEALTH at most!"
      ]
    , [ "Why did you go and buy that?  Ridiculous!  I would have spent my " <>
        "WEALTH on GOOD CHEESE instead."
      ]
    , [ "Keep on buying stuff, MINION!  The owner of THE SHOP is a FAIRY " <>
        "and a good friend of mine."
      ]
    , [ "Doesn't it feel great to buy THINGS?" ]
    , [ "Oh, so many THINGS you can buy." ]
    , [ "I wish THE SHOP sold COMPETENT MINIONS." ]
    , [ "Hmm.  I wonder what you're planning on using " <> an card.name <>
        " for."
      ]
    , [ "That is a very mediocre-looking " <> card.name <> "." ]
    , [ "That " <> card.name <> " just looks boring. Try composing it with " <>
        "something more interesting."
      ]
    , [ "Yes, better buy some " <> card.name <> " before they sell out." ]
    , [ "Buy! Buy! Buy!" ]
    ]
genericDialog (Sell card) = liftEffect $ pick
    [ [ "Uh-oh!  I'm not sure if selling that was a wise decision.  It's " <>
        "common around here to get in a dangerous situation where " <>
        an card.name <> " is the only thing that can save you."
      ]
    , [ "Bye-bye, " <> card.name <> "!  Hello, WEALTH!" ]
    , [ "Yeah, that " <> card.name <> " was useless anyway." ]
    , [ "Don't forget that there are more important things in life than " <>
        "WEALTH. For example, great THINGS like " <> an card.name <> "." <>
        "But too late now, you sold it."
      ]
    , [ "Selling THINGS is fine, but only if you're doing it to buy " <>
        "new THINGS."
      ]
    , [ "Why would you sell " <> an card.name <> "?  It is, like, in the " <>
        "top 10 of useful THINGS to have!"
      ]
    ]
genericDialog (Compose comp) = liftEffect $ pick
    [ [ "Wow, I actually would have never thought to put " <> an comp.left <>
        " together with... THAT.  You are either very creative, or, well, " <>
        "you just have a sick, sick mind."
      ]
    , [ "Not bad, MINION." ]
    , [ "That looks... mildly interesting." ]
    , [ "What are you trying to do here?  You realize that there is no " <>
        "goal to BEERAFFE, right?"
      ]
    , [ "Composing THINGS was invented by the ANCIENT PURPLE ELDER DRAGON " <>
        "when he accidentally composed his GRANDMA with a PIZZA he was eating."
      ]
    , [ "I once composed a MINION like you with another MINION.  The result " <>
        "was, unsurprisingly, very disappointing."
      ]
    , [ "Oh, gross, gross!  Let me take a picture to post on my WITCHSTAGRAM."
      ]
    , [ "Hah!  Reminds me of that time I composed someone who left an angry " <>
        "comment on my page with " <> an comp.right <> "."
      ]
    , [ "You're finally making some progress, MINION.  You might master " <>
        "PURPLE MAGIC yet."
      ]
    , [ "That's a big " <> comp.name <> "." ]
    ]
