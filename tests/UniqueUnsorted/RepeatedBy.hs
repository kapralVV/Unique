module UniqueUnsorted.RepeatedBy where

import Test.Hspec
import Test.QuickCheck

import Data.List.UniqueUnsorted
import Data.List (group, sort)


repeatedByTests :: SpecWith ()
repeatedByTests =
  describe "Data.List.UniqueUnsorted.repeatedBy,repeated,unique" $ do
  
  it "repeatedBy: should return [] with empty list" $ do
    repeatedBy (>100) ( [] :: [Int] ) `shouldBe` []

  it "repeatedBy: simple test" $ do
    sort (repeatedBy (>2) "This is the test line") `shouldBe` " eist"

  it "repeatedBy: returns [] when predicate (=< negative) " $
    property $
    \x xs -> x < 0
             ==> null ( repeatedBy (<= x) (xs :: String) )

  it "repeatedBy: returns [] when preficate (== 0)" $
     property $
     \ xs -> null ( repeatedBy (== 0) (xs :: [Int]) )

  it "repeatedBy: resulted elements should occur only once" $
    property $
    \ x xs -> x > 0
              ==> all ((==1) . length) . group $ repeatedBy (> x) ( xs :: String )

  it "unique: simple test" $ do
    sort (unique  "foo bar") `shouldBe` " abfr"

  it "repeated: simple test" $ do
    repeated  "foo bar" `shouldBe` "o"

  it "unique: multiple execution should return the same result" $
    property $
    \ xs -> unique (xs :: String) == unique (unique xs)

  it "repeated: multiple execution should return []" $
    property $
    \ xs -> null (repeated (repeated (xs :: String) ))

  it "repeated && unique: should return different result on the same non empty list" $
    property $
    \ xs -> not (null xs)
            ==> repeated (xs :: String) /= unique xs
