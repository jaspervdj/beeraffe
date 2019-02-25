-- | Working with a stack of 2D boxes and some simple gravity.
module Beeraffe.Stack
    ( Stack
    , empty
    , lookup
    , delete
    , top
    , unsafeInsert
    , drop
    , step
    ) where

import Beeraffe.BinarySearch as BS
import Beeraffe.Linear.Box (Box)
import Beeraffe.Linear.Box as Box
import Beeraffe.Linear.V2 (mkV2)
import Data.Tuple (Tuple (..))
import Beeraffe.Linear.V2 as V2
import Data.Foldable (minimum)
import Data.List as List
import Data.FoldableWithIndex (allWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Prelude

type Stack k =
    { boxes  :: Map.Map k (Box Int)
    , width  :: Int
    , bottom :: Int
    }

-- | Create a new, empty stack with the specified dimensions.
empty :: forall k. Int -> Int -> Stack k
empty width bottom = {boxes: Map.empty, width, bottom}

-- | Lookup the box for a certain item.
lookup :: forall k. Ord k => k -> Stack k -> Maybe (Box Int)
lookup k stack = Map.lookup k stack.boxes

-- | Remove a box from the stack.
delete :: forall k. Ord k => k -> Stack k -> Stack k
delete k stack = stack {boxes = Map.delete k stack.boxes}

-- | Find the top of the stack.  This can be a negative number in case the
-- height is exceeded.
top :: forall k. Stack k -> Int
top stack = fromMaybe stack.bottom $ minimum $
    map (\b -> V2.y (b.topLeft)) stack.boxes

-- | Insert a box at a location in the stack.  This box should not overlap with
-- other boxes, and it should not "float".  Neither of these preconditions are
-- checked.
unsafeInsert :: forall k. Ord k => k -> Box Int -> Stack k -> Stack k
unsafeInsert k box stack = stack {boxes = Map.insert k box stack.boxes}

-- | Drops a (new or old) box as far down as possible.  Returns the updated
-- stack and the new position of the box.
drop
    :: forall k. Ord k
    => k -> Box Int -> Stack k -> {box :: Box Int, stack :: Stack k}
drop k box stack =
    let bottomy = BS.upwardsBinarySearch isOk startby
        dropped = Box.move (mkV2 0 (bottomy - startby)) box in
    {box: dropped, stack: stack {boxes = Map.insert k dropped stack.boxes}}
  where
    startbx = V2.x box.bottomRight
    startby = V2.y box.bottomRight
    isOk y  =
        y < stack.bottom &&
        allWithIndex
            (\l b ->
                l == k ||
                not (Box.intersects b (box {bottomRight = mkV2 startbx y})))
            stack.boxes

-- | Drops all blocks, returns the blocks that were moved and the new stack.
step
    :: forall k. Ord k
    => Stack k -> {dropped :: Map.Map k (Box Int), stack :: Stack k}
step stack = List.foldl
    (\acc (Tuple k box) ->
        let res = drop k box acc.stack in
        { dropped: if res.box == box
                    then acc.dropped
                    else Map.insert k res.box acc.dropped
        , stack:   res.stack
        })
    {dropped: Map.empty, stack: stack}
    -- Start with dropping the lowest boxes.
    (List.sortBy
        (\(Tuple _ l) (Tuple _ r) ->
            compare (V2.y r.bottomRight) (V2.y l.bottomRight) ) $
        Map.toUnfoldable stack.boxes)
