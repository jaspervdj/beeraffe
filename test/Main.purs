module Test.Main where

import Beeraffe.BinarySearch.Tests as Beeraffe.BinarySearch.Tests
import Beeraffe.Compose.Tests as Beeraffe.Compose.Tests
import Beeraffe.Linear.Box.Tests as Beeraffe.Linear.Box.Tests
import Beeraffe.Stack.Tests as Beeraffe.Stack.Tests
import Effect (Effect)
import Prelude

main :: Effect Unit
main = do
    Beeraffe.BinarySearch.Tests.tests
    Beeraffe.Compose.Tests.tests
    Beeraffe.Linear.Box.Tests.tests
    Beeraffe.Stack.Tests.tests
