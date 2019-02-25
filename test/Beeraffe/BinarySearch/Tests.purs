module Beeraffe.BinarySearch.Tests
    ( tests
    ) where

import Beeraffe.BinarySearch as BS
import Effect (Effect)
import Prelude
import Test.Assert as TA

tests :: Effect Unit
tests = do
    TA.assertEqual
        { actual: BS.upwardsBinarySearch (\x -> x <= 10) 3
        , expected: 10
        }
    TA.assertEqual
        { actual: BS.upwardsBinarySearch (\x -> x <= 80) 80
        , expected: 80
        }
    TA.assertEqual
        { actual: BS.upwardsBinarySearch (\x -> x < 0) 0
        , expected: 0
        }
    TA.assertEqual
        { actual: BS.upwardsBinarySearch (\x -> x <= 100000) 0
        , expected: 100000
        }
