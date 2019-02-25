module Beeraffe.BinarySearch
    ( binarySearch
    , upwardsBinarySearch
    ) where

import Prelude

binarySearch
    :: (Int -> Boolean)  -- ^ Predicate
    -> Int               -- ^ Known "good" position
    -> Int               -- ^ Known "bad" position
    -> Int               -- ^ Maximal "good" position
binarySearch f good bad
    | good + 1 >= bad = good
    | otherwise       =
        let test = (good + bad) `div` 2 in
        if f test
            then binarySearch f test bad
            else binarySearch f good test

upwardsBinarySearch
    :: (Int -> Boolean)  -- ^ Predicate
    -> Int               -- ^ Known "good" position
    -> Int               -- ^ Maximal "good" position
upwardsBinarySearch f good0 = go good0 1
  where
    go good n =
        let test = good0 + n in
        if f test
            then go test (n * 2)
            else binarySearch f good test
