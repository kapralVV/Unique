module Unique.Complex where

import Test.Hspec
import Test.QuickCheck

import Data.List.Unique
import Data.List (sort)


triplet :: (a -> b) -> (a, a, a) -> (b, b, b)
triplet f (x, y, z) = (f x, f y, f z)

complexTests :: SpecWith ()
complexTests =
  describe "Data.List.Unique.complex" $ do
  
  it "complex: should return ([],[],[]) with empty list" $ do
    complex ( [] :: [Int] ) `shouldBe` ([],[],[])

  it "complex: simple test" $ do
    complex "This is the test line" `shouldBe` ("This teln","is hte","Tln")

  it "complex: returns the same result as `sortUniq`, `repeated`, `unique` but not sorted" $
    property $
    \ xs -> triplet sort ( complex (xs :: String) )
            == (sortUniq xs, repeated xs, unique xs)
