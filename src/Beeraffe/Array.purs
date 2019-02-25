module Beeraffe.Array
    ( shuffle
    , insertAt
    , pick
    ) where

import Data.Array as Array
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Random as Random
import Partial.Unsafe (unsafePartial)
import Prelude

foreign import shuffle :: forall a. Array a -> Effect (Array a)

-- | Insert an element at the specified position in the array.  If that fails,
-- put it in the front.
insertAt :: forall a. Int -> a -> Array a -> Array a
insertAt idx x arr = case Array.insertAt idx x arr of
    Nothing   -> Array.cons x arr
    Just arr' -> arr'

-- | Pick a random element from the array.
pick :: forall a. Array a -> Effect a
pick arr = do
    next <- Random.randomInt 0 (Array.length arr - 1)
    pure $ unsafePartial $ Array.unsafeIndex arr next
