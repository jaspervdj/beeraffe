module Beeraffe.UI.Notification
    ( Query (..)
    , component
    ) where

import Data.Array as Array
import Data.Maybe (Maybe (..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude

type State =
    { queue      :: Array (Array String)
    , transition :: TransitionState
    }

data TransitionState
    = Done
    | Showing
    | Hiding

data Query a
    = Push (Array String) a
    | TransitionEnd a

component
    :: forall m. MonadAff m => H.Component HH.HTML Query Unit Void m
component = H.component
    { initialState: const {queue: [], transition: Done}
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render state = HH.div
        [ HP.classes $
            [HH.ClassName "notification"] <>
            (case state.transition of
                Showing -> [HH.ClassName "show"]
                _       -> [])
        , HE.onTransitionEnd $ HE.input \_ x -> (TransitionEnd x)
        ]
        (case Array.index state.queue 0 of
            Nothing -> []
            Just lines -> map (\l -> HH.p [] [HH.text l]) lines)

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of
        Push lines next -> do
            H.modify_ $ \s -> s {queue = Array.snoc s.queue lines}

            -- NOTE(jaspervdj): The following line ensures that we first set the
            -- text in the notification (so the height of the element is
            -- correct) before triggering the transition.  If we don't do this,
            -- the transition uses the wrong height on iOS/Safari.
            H.liftAff $ Aff.delay $ Aff.Milliseconds 1.0

            startAnimation
            pure next
        TransitionEnd next -> do
            H.modify_ $ \s -> case s.transition of
                Done -> s
                Showing -> s {transition = Hiding}
                Hiding  -> s {transition = Done, queue = pop s.queue}

            H.liftAff $ Aff.delay $ Aff.Milliseconds 1.0
            startAnimation
            pure next

    -- Usually we only remove one item from the notification queue, but if it's
    -- completely clogged up we'll drop 5 and add a "...".
    pop queue
        | Just {head, tail} <- Array.uncons (Array.drop 5 queue) =
            Array.cons (["..."] <> head) tail
        | otherwise =
            Array.drop 1 queue

    startAnimation = do
        state <- H.get
        case state.transition of
            Done | not (Array.null state.queue) ->
                H.modify_ $ \s -> s {transition = Showing}
            _ -> pure unit
