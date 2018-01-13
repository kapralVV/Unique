module AllUnique where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List (nub)

import Data.List.Unique


allUnique' :: Eq a => [a] -> Bool
allUnique' ls = (nub ls) == ls

allUniqueTests :: SpecWith ()
allUniqueTests =
  describe "Data.List.Unique.allUnique" $ do
  it "allUnique: returns True with empty list" $ do
    allUnique "" `shouldBe` True

  it "allUnique: returns False when some element is not unique" $ do
    allUnique "foo bar" `shouldBe` False

  it "allUnique: returns True when list does not have duplicates" $ do
    allUnique ([1..1000] :: [Int]) `shouldBe` True

  it "allUnique: fails if list consist of duplicate elements and 'undefined' after them" $ do
    evaluate (allUnique ['a', 'a', undefined] )
      `shouldThrow`
      errorCall "Prelude.undefined"

  it "allUnique: Test the function using slower analog and random data" $
    property $
    \ xs -> (allUnique' (xs :: String)) == (allUnique xs)
