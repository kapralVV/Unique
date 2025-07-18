module UniqueUnsorted.RemoveDuplicates where

import Test.Hspec
import Test.QuickCheck

import Data.List.UniqueUnsorted
import Data.List (group, sort)


removeDuplicatesTests :: SpecWith ()
removeDuplicatesTests =
  describe "Data.List.UniqueUnsorted.removeDuplicates" $ do
  
  it "removeDuplicates: should return [] with empty list" $ do
    removeDuplicates ( [] :: [Int] ) `shouldBe` []

  it "removeDuplicates: simple test" $ do
    sort (removeDuplicates "foo bar") `shouldBe` " abfor"

  it "removeDuplicates: multiple execution should return the same result" $
    property $
    \ xs -> removeDuplicates (removeDuplicates ( xs :: String) ) == removeDuplicates xs

  it "removeDuplicates: elements should occur only once" $
    property $
    \ ( NonEmpty ls@(x:_) ) -> isUnique (x :: Float) (removeDuplicates ls) == Just True

  it "removeDuplicates: elements should occur only once #2" $
    property $
    \ xs -> all ((==1) . length) . group $ removeDuplicates ( xs :: [Integer] )
