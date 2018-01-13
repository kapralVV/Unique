-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.UniqueStrict
-- Copyright   :  (c) Volodymyr Yaschenko
-- License     :  BSD3
--
-- Maintainer  :  ualinuxcn@gmail.com
-- Stability   :  Unstable
-- Portability :  portable
--
-- Library provides functions to find unique and duplicate elements in the list.
-- Unlike Data.List.Unique this one uses Data.Map.Strict for calculations.
-- So it's much faster and it uses less memory.

module Data.List.UniqueStrict
        (
          repeated
        , repeatedBy
        , unique
        , count
        , count_ )
        where

import qualified Data.Map.Strict    as MS (Map, filter, fromListWith, keys,
                                           toList)

import qualified Data.IntMap.Strict as IM (fromListWith, toList)

import qualified Data.List          as L (sort)

countMap :: Ord a => [a] -> MS.Map a Int
countMap = MS.fromListWith (+) . flip zip (repeat 1)

-- | The 'repeatedBy' function behaves just like repeated, except it uses a user-supplied equality predicate.
--
-- > repeatedBy (>2) "This is the test line" == " eist"

repeatedBy :: Ord a => (Int -> Bool) -> [a] -> [a]
repeatedBy p = MS.keys . MS.filter p . countMap

-- | 'repeated' finds only the elements that are present more than once in the list. Example:
--
-- > repeated  "foo bar" == "o"

repeated :: Ord a => [a] -> [a]
repeated = repeatedBy (>1)

-- | 'unique' gets only unique elements, that do not have duplicates.
-- It sorts them. Example:
--
-- > unique  "foo bar" == " abfr"

unique :: Ord a => [a] -> [a]
unique = repeatedBy (==1)

-- | 'count' of each element in the list, it sorts by keys (elements). Example:
--
-- > count "foo bar" == [(' ',1),('a',1),('b',1),('f',1),('o',2),('r',1)]

count :: Ord a => [a] -> [(a, Int)]
count = MS.toList . countMap

-- | 'count_' of each elements in the list, it sorts by their number. Example:
--
-- > count_ "foo bar" == [(' ',1),('a',1),('b',1),('f',1),('r',1),('o',2)]

count_ :: Ord a => [a] -> [(a, Int)]
count_ = fromIntMap . toIntMap . MS.toList . countMap
    where toIntMap = IM.fromListWith (++) . map (\(x,y) -> (y,[x]))
          fromIntMap = concatMap (\(x,y) -> L.sort . zip y $ repeat x) . IM.toList
