-- | Stupid wrapper to get our assets from webpack.
module Beeraffe.Assets
    ( css
    , sprites
    , sorceress
    , words
    , loadCSS
    ) where

import Effect (Effect)
import Prelude

foreign import css       :: String
foreign import sprites   :: String
foreign import sorceress :: String
foreign import words     :: String

foreign import loadCSS :: String -> Effect Unit
