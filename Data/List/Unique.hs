-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Unique
-- Copyright   :  (c) Volodymyr Yashchenko
-- License     :  BSD3
--
-- Maintainer  :  ualinuxcn@gmail.com
-- Stability   :  Unstable
-- Portability :  portable
--
-- Library provides the functions to find unique and duplicate elements in the list

module Data.List.Unique
   ( uniq
   , complex
   , isUnique
   , isRepeated
   , sortUniq
   , repeated
   , repeatedBy
   , unique
   , allUnique
   , count
   , count_
   , occurrences
   , countElem
   )
   where


import           Data.List           (group, sort, sortBy, (\\))

import           Control.Applicative (liftA2)
import           Data.Function       (on)
import           Data.List.Extra     (nubOrd)
import           Data.Tuple          (swap)

-- | 'uniq' behaves the same as 'uniq uniq' utility does (without cli additional options)
--
-- > uniq "1121331" == "12131"

uniq :: Eq b => [b] -> [b]
uniq = map head . group

-- | 'sortUniq' sorts the list and removes the duplicates of elements. Example:
--
-- > sortUniq "foo bar" == " abfor"

sortUniq :: Ord a => [a] -> [a]
sortUniq = sort . nubOrd

sg :: Ord a => [a] -> [[a]]
sg = group . sort

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = filter (p . length) . sg

-- | 'repeated' finds only the elements that are present more than once in the list. Example:
--
-- > repeated  "foo bar" == "o"

repeated :: Ord a => [a] -> [a]
repeated = repeatedBy (>1)

-- | The repeatedBy function behaves just like repeated, except it uses a user-supplied equality predicate.
--
-- > repeatedBy (>2) "This is the test line" == " eist"

repeatedBy :: Ord a => (Int -> Bool) -> [a] -> [a]
repeatedBy p = map head . filterByLength p

-- | 'unique' gets only unique elements, that do not have duplicates.
-- It sorts them. Example:
--
-- > unique  "foo bar" == " abfr"

unique :: Ord a => [a] -> [a]
unique = concat . filterByLength (==1)

lh :: [a] -> (a, Int)
lh = liftA2 (,) head length

-- | 'allUnique' checks whether all elements of the list are unique
--
-- > allUnique "foo bar" == False
-- > allUnique ['a'..'z'] == True
-- > allUnique [] == True (!)
-- Since 0.4.7

allUnique :: Ord a => [a] -> Bool
allUnique = all ( (==) 1 . length) . sg

-- | 'count' of each element in the list, it sorts by keys (elements). Example:
--
-- > count "foo bar" == [(' ',1),('a',1),('b',1),('f',1),('o',2),('r',1)]

count :: Ord a => [a] -> [(a, Int)]
count = map lh . sg

-- | 'count_' of each elements in the list, it sorts by their number. Example:
--
-- > count_ "foo bar" == [(' ',1),('a',1),('b',1),('f',1),('r',1),('o',2)]

count_ :: Ord a => [a] -> [(a, Int)]
count_ = sortBy (compare `on` snd) . count

-- | 'countElem' gets the number of occurrences of the specified element. Example:
--
-- > countElem 'o' "foo bar" == 2

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)

-- | 'complex' function is a complex investigation of the list. It returns triple:
--
-- * the first - all elements with removed duplicates (like 'sortUniq' but the result is not sorted)
--
-- * the second - the elements that are repeated at least once in the list (result is the same as 'repeated' but not sorted)
--
-- * the third - the unique elements that do not have duplicates (result is the same as 'unique' but not sorted)
--
-- 'complex' does not sort the resulted elements of triple as well as it can be used for types that does not have Ord instance.
--
-- Anyway, it's better to use 'sortUniq', 'repeated' and 'unique' instead of 'complex' when type 'a' has Ord instance.
--
-- > complex "This is the test line" == ("This teln","is hte","Tln")
--
-- Since 0.4.4
--

complex :: Eq a => [a] -> ([a], [a], [a])
complex = triplet reverse . (\(z,y) ->  (z, y, z \\ y )) . go ([], [])
    where
      go (occurred, repeated') [] = (occurred, repeated')
      go (occurred, repeated') (x:xs)
          | x `elem` repeated' = go (occurred, repeated')   xs
          | x `elem` occurred  = go (occurred, x:repeated') xs
          | otherwise        = go (x:occurred, repeated') xs

triplet :: (a -> b) -> (a, a, a) -> (b, b, b)
triplet f (x, y, z) = (f x, f y, f z)

merge :: Eq a => [(a,b)] -> [(a,[b])]
merge [] = []
merge ((x,y):xs) = (x, y : map snd ys) : merge zs
    where (ys,zs) = span ( (== x) . fst) xs

-- | 'occurrences' like 'count' or 'count_' but shows the list of elements that occur X times
--
-- > occurrences "This is the test line" == [(1,"Tln"),(2,"h"),(3,"eist"),(4," ")]
-- Since 0.4.4
--

occurrences :: Ord a => [a] -> [(Int, [a])]
occurrences = merge . map swap . count_

-- | 'isUnique' function is to check whether the given element is unique in the list or not.
--
-- It returns Nothing when the element does not present in the list. Examples:
--
-- > isUnique 'f' "foo bar" == Just True
-- > isUnique 'o' "foo bar" == Just False
-- > isUnique '!' "foo bar" == Nothing
--
-- Since 0.4.5
--

isUnique :: Eq a => a -> [a] -> Maybe Bool
isUnique a = go Nothing a
    where go s _ [] = s
          go s@Nothing x (z:zs)
            | x == z = go (Just True) x zs
            | otherwise = go s x zs
          go s@(Just True) x (z:zs)
            | x == z = Just False
            | otherwise = go s x zs
          go s@(Just False) _ _ = s

-- | 'isRepeated' is a reverse function to 'isUnique'
--
-- Since 0.4.5
isRepeated :: Eq a => a -> [a] -> Maybe Bool
isRepeated x = fmap not . isUnique x
