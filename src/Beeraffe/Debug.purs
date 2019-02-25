module Beeraffe.Debug
    ( appendImage
    , appendParagraph
    , appendBenchmark
    ) where

import Beeraffe.Image (Image)
import Prelude
import Effect (Effect)

foreign import appendImage :: Image -> Effect Unit
foreign import appendParagraph :: String -> Effect Unit
foreign import appendBenchmark :: forall a. String -> (Unit -> a) -> Effect a
