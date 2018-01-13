module UniqueUnsorted.RemoveDuplicates where

import Test.Hspec
import Test.QuickCheck

import Data.List.UniqueUnsorted
import Data.List (sort, group)


removeDuplicatesTests :: SpecWith ()
removeDuplicatesTests =
  describe "Data.List.UniqueUnsorted.removeDuplicates" $ do
  
  it "removeDuplicates: should return [] with empty list" $ do
    removeDuplicates ( [] :: [Int] ) `shouldBe` []

  it "removeDuplicates: simple test" $ do
    removeDuplicates "foo bar" `shouldBe` " abrfo"

  it "removeDuplicates: multiple execution should return the same result" $
    property $
    \ xs -> removeDuplicates (removeDuplicates ( xs :: String) ) == removeDuplicates xs

  it "removeDuplicates: elements should occur only once" $
    property $
    \ ( NonEmpty ls@(x:_) ) -> isUnique (x :: Float) (removeDuplicates ls) == Just True

  it "removeDuplicates: elements should occur only once #2" $
    property $
    \ xs -> all (==1) . map length . group $ removeDuplicates ( xs :: [Integer] )
