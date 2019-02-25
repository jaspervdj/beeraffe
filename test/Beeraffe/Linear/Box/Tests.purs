module Beeraffe.Linear.Box.Tests
    ( tests
    ) where

import Beeraffe.Linear.Box (mkBox)
import Beeraffe.Linear.Box as Box
import Beeraffe.Linear.V2 (mkV2)
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Prelude
import Test.Assert as TA

tests :: Effect Unit
tests = do
    TA.assertEqual
        { actual: Box.intersection (mkBox (mkV2 0 0)) (mkBox (mkV2 1 1))
        , expected: Nothing
        }

    TA.assertEqual
        { actual:
            let a = mkBox (mkV2 1 3) `Box.append` mkBox (mkV2 6 9)
                b = mkBox (mkV2 4 0) `Box.append` mkBox (mkV2 8 10) in
            Box.intersection a b
        , expected: Just $ mkBox (mkV2 4 3) `Box.append` mkBox (mkV2 6 9)
        }

    TA.assertEqual
        { actual:
            let a = mkBox (mkV2 0 0) `Box.append` mkBox (mkV2 6 9)
                b = mkBox (mkV2 (-4) (-2)) `Box.append` mkBox (mkV2 8 10) in
            Box.intersection a b
        , expected: Just $ mkBox (mkV2 0 0) `Box.append` mkBox (mkV2 6 9)
        }
