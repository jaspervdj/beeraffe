module Beeraffe.Compose
    ( composeSprites
    , composeWords
    ) where

import Beeraffe.Compose.Sprites (mipmapped, render)
import Beeraffe.Sprite (Sprite)
import Beeraffe.Sprite as Sprite
import Control.Alternative ((<|>), empty)
import Control.MonadZero (guard)
import Data.Maybe (Maybe)
import Data.String as String
import Prelude

composeSprites :: Sprite -> Sprite -> Sprite
composeSprites l r =
    let sprite = render (mipmapped l r) in
    if sprite.width < sprite.height
        then Sprite.rotateClockwise sprite
        else sprite

composeWords
    :: String -> String
    -> Maybe
        { left   :: String
        , right  :: String
        , result :: String
        , parts  :: {l :: String, m :: String, r :: String}
        }
composeWords x y
    -- We never allow merging identical words.
    | x == y    = empty
    | otherwise = compose x y <|> compose y x
  where
    compose l r =
        composeAt 0 l r <|> composeAt 1 l r <|> composeAt 2 l r

    composeAt lidx l r = do
        let llen = String.length l
        guard $ lidx < llen
        lc <- String.codePointAt (llen - lidx - 1) l
        let needle = String.singleton lc
        ridx <- String.indexOf (String.Pattern needle) r
        guard $ ridx <= 2

        let lpart    = String.take (llen - lidx - 1) l
            rpart    = String.drop (ridx + 1) r
            proposed = lpart <> needle <> rpart

        guard $ proposed /= l && proposed /= r
        guard $ not $ String.null lpart || String.null rpart
        pure
            { left:   l
            , right:  r
            , result: proposed
            , parts:  {l: lpart, m: needle, r: rpart}
            }
