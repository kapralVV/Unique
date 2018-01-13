-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.UniqueUnsorted
-- Copyright   :  (c) Volodymyr Yashchenko
-- License     :  BSD3
--
-- Maintainer  :  ualinuxcn@gmail.com
-- Stability   :  Unstable
-- Portability :  portable
--
-- Library provides functions to find unique and duplicate elements in the list.
-- Unlike Unique or UniqueStrict modules this one uses Data.HashMap.Strict for calculation.
--
-- The elements in the list can be unsorted (do not have an instance of Ord class, but Hashable is needed).
-- This implementation is good for ByteStrings.

module Data.List.UniqueUnsorted
        ( isUnique
        , isRepeated
        , removeDuplicates
        , repeated
        , repeatedBy
        , unique
        , allUnique
        , count
        , count_
        )
        where

import           Data.Hashable
import qualified Data.HashMap.Strict as HS (HashMap, filter, fromListWith, keys,
                                            toList, lookup, map, foldr)

import qualified Data.HashSet        as DHS (toList, fromList)

import qualified Data.IntMap.Strict  as IM (fromListWith, toList)


countMap :: (Hashable a, Eq a) => [a] -> HS.HashMap a Int
countMap = HS.fromListWith (+) . flip zip (repeat 1)

-- | 'isUnique' function is to check whether the given element is unique in the list or not.
--
-- It returns Nothing when the element does not present in the list. Examples:
--
-- > isUnique 'f' "foo bar" == Just True
-- > isUnique 'o' "foo bar" == Just False
-- > isUnique '!' "foo bar" == Nothing
--
-- Since 0.4.7.2
--

isUnique :: (Hashable a, Eq a) => a -> [a] -> Maybe Bool
isUnique x = fmap (== 1) . HS.lookup x . countMap

-- | 'isRepeated' is a reverse function to 'isUnique'
--
-- Since 0.4.7.2

isRepeated :: (Hashable a, Eq a) => a -> [a] -> Maybe Bool
isRepeated x = fmap not . isUnique x

-- | 'removeDuplicates' removes the duplicates of elements. Example:
--
-- > removeDuplicates "foo bar" == " abrfo"

removeDuplicates :: (Hashable a, Eq a) => [a] -> [a]
removeDuplicates = DHS.toList . DHS.fromList

-- | The 'repeatedBy' function behaves just like 'repeated', except it uses a user-supplied equality predicate.
--
-- > repeatedBy (>2) "This is the test line" == " stei"

repeatedBy :: (Hashable a, Eq a) => (Int -> Bool) -> [a] -> [a]
repeatedBy p = HS.keys . HS.filter p . countMap

-- | 'repeated' finds only the elements that are present more than once in the list. Example:
--
-- >  repeated  "foo bar" == "o"

repeated :: (Hashable a, Eq a) => [a] -> [a]
repeated = repeatedBy (>1)

-- | 'unique' gets only unique elements, that do not have duplicates.
--
-- > unique  "foo bar" == " abrf"

unique :: (Hashable a, Eq a) => [a] -> [a]
unique = repeatedBy (==1)


-- | 'allUnique' checks whether all elements of the list are unique
--
-- > allUnique "foo bar" == False
-- > allUnique ['a'..'z'] == True
-- > allUnique [] == True (!)
-- Since 0.4.7.2

allUnique :: (Hashable a, Eq a) => [a] -> Bool
allUnique = HS.foldr (&&) True . HS.map (==1) . countMap

-- | 'count' of each element in the list. Example:
--
-- > count "This is the test line" == [(' ',4),('s',3),('T',1),('t',3),('e',3),('h',2),('i',3),('l',1),('n',1)]

count :: (Hashable a, Eq a) => [a] -> [(a, Int)]
count = HS.toList . countMap

-- | 'count_' of each elements in the list, it sorts by their number. Example:
--
-- >  count_ "This is the test line" == [('n',1),('l',1),('T',1),('h',2),('i',3),('e',3),('t',3),('s',3),(' ',4)]

count_ :: (Hashable a, Eq a) => [a] -> [(a, Int)]
count_ = fromIntMap . toIntMap . HS.toList . countMap
    where toIntMap   = IM.fromListWith (++) . map (\(x,y) -> (y,[x]))
          fromIntMap = concatMap (\(x,y) -> zip y $ repeat x) . IM.toList
