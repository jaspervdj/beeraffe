-- | Wraps image loading to do some operations we want to support.
module Beeraffe.Image
    ( Image ()
    , load
    , fromPixelData
    , width
    , height
    , pixelData
    ) where

import Data.Function.Uncurried (Fn3, Fn5)
import Effect.Aff.Compat (EffectFnAff)

foreign import data Image    :: Type
foreign import load          :: String -> EffectFnAff Image
foreign import fromPixelData :: Fn3 Int Int (Array Int) Image
foreign import width         :: Image -> Int
foreign import height        :: Image -> Int
foreign import pixelData     :: Fn5 Int Int Int Int Image (Array Int)
