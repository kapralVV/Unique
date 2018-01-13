module UniqueStrict.SortUniq where

import Test.Hspec
import Test.QuickCheck

import Data.List.UniqueStrict
import Data.List (sort, group)
import Data.Set (fromList, toList)


sortUniqTests :: SpecWith ()
sortUniqTests =
  describe "Data.List.UniqueStrict.sortUniq" $ do
  
  it "sortUniq: should return [] with empty list" $ do
    sortUniq ( [] :: [Int] ) `shouldBe` []

  it "sortUniq: simple test" $ do
    sortUniq "foo bar" `shouldBe` " abfor"

  it "sortUniq: multiple execution should return the same result" $
    property $
    \ xs -> sortUniq (sortUniq ( xs :: String) ) == sortUniq xs

  it "sortUniq: the result has to be sorted" $
    property $
    \ xs -> sortUniq ( xs :: [Int] ) == sort (sortUniq xs)

  it "sortUniq: elements should occur only once" $
    property $
    \ ( NonEmpty ls@(x:_) ) -> isUnique (x :: Float) (sortUniq ls) == Just True

  it "sortUniq: elements should occur only once #2" $
    property $
    \ xs -> all (==1) . map length . group $ sortUniq ( xs :: [Integer] )

  it "sortUniq: check if it's correct by slow analog function" $
    property $
    \ xs -> ( map head . group . sort $ (xs :: String) )
            == sortUniq xs

  it "sortUniq: check if it's correct by the Faster analog function :)" $
    property $
    \ xs -> sortUniq ( xs :: String ) == toList (fromList xs)
