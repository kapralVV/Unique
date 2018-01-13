-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.UniqueStrict
-- Copyright   :  (c) Volodymyr Yashchenko
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
        ( sortUniq
        , isUnique
        , isRepeated
        , repeated
        , repeatedBy
        , unique
        , allUnique
        , count
        , count_ )
        where

import qualified Data.Map.Strict    as MS (Map, filter, fromListWith, keys,
                                           toList, lookup, map, foldr)

import qualified Data.IntMap.Strict as IM (fromListWith, toList)

import qualified Data.Set           as DS (fromList, toList)


countMap :: Ord a => [a] -> MS.Map a Int
countMap = MS.fromListWith (+) . flip zip (repeat 1)

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

isUnique :: Ord a => a -> [a] -> Maybe Bool
isUnique x = fmap (== 1) . MS.lookup x . countMap

-- | 'isRepeated' is a reverse function to 'isUnique'
--
-- Since 0.4.5

isRepeated :: Ord a => a -> [a] -> Maybe Bool
isRepeated x = fmap not . isUnique x

-- | 'sortUniq' sorts the list and removes the duplicates of elements. Example:
--
-- > sortUniq "foo bar" == " abfor"

sortUniq :: Ord a => [a] -> [a]
sortUniq = DS.toList . DS.fromList

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

-- | 'allUnique' checks whether all elements of the list are unique
--
-- > allUnique "foo bar" == False
-- > allUnique ['a'..'z'] == True
-- > allUnique [] == True (!)
-- Since 0.4.7.2

allUnique :: Ord a => [a] -> Bool
allUnique = MS.foldr (&&) True . MS.map (==1) . countMap

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
          fromIntMap = concatMap (\(x,y) -> sortUniq . zip y $ repeat x) . IM.toList
