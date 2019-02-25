module Beeraffe.Compose.Tests
    ( tests
    ) where

import Beeraffe.Compose (composeWords)
import Data.Maybe (Maybe (..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Prelude
import Test.Assert as TA

tests :: Effect Unit
tests = traverse_ test
    [ {l: "beer", r: "giraffe", e: Just "beeraffe"}
    , {l: "giraffe", r: "beer", e: Just "giraffeer"}
    , {l: "absurd", r: "nonsense", e: Just "nonsensurd"}
    , {l: "ball", r: "cheese", e: Nothing}
    , {l: "banana", r: "banana", e: Nothing}
    -- NOTE(jaspervdj): This case is problematic because the naive composition
    -- of "beer" and "bento" would be "bento"; we want to avoid mapping back to
    -- one of the two inputs.
    , {l: "beer", r: "bento", e: Just "beento"}
    ]
  where
    test t = TA.assertEqual {actual: composeWords t.l t.r, expected: t.e}
