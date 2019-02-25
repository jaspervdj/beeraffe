module Beeraffe.Stack.Tests
    ( tests
    ) where

import Data.Tuple (Tuple (..))
import Beeraffe.Linear.Box (mkBox)
import Beeraffe.Linear.Box as Box
import Beeraffe.Linear.V2 (mkV2)
import Beeraffe.Stack as Stack
import Data.Map as Map
import Effect (Effect)
import Prelude
import Test.Assert as TA

tests :: Effect Unit
tests = do
    TA.assertEqual
        { actual: Stack.top (Stack.empty 1000 1000)
        , expected: 1000
        }

    let qbox tx ty bx by = mkBox (mkV2 tx ty) `Box.append` mkBox (mkV2 bx by)

    TA.assertEqual
        { actual:
            let s0 = Stack.empty 128 128
                s1 = (Stack.drop "hello" (qbox 0 0 31 31) s0).stack
                s2 = (Stack.drop "world" (qbox 32 0 63 31) s1).stack
                s3 = (Stack.drop "above" (qbox 16 0 50 31) s2).stack in
             s3.boxes
        , expected: Map.fromFoldable
            [ Tuple "hello" (qbox 0 96 31 127)
            , Tuple "world" (qbox 32 96 63 127)
            , Tuple "above" (qbox 16 64 50 95)
            ]
        }
