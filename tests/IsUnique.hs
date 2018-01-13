module IsUnique where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Maybe (isJust, isNothing)

import Data.List.Unique

isUniqueTests :: SpecWith ()
isUniqueTests =
  describe "Data.List.Unique.isUnique" $ do
  it "isUnique should return Nothing with empty list" $ do
    isUnique (1 :: Int) [] 
    `shouldBe`
      Nothing

  it "isUnique should return Nothing with empty list (quickCheck)" $
    property $
    \x -> isNothing $ isUnique (x :: Char) []

  it "isUnique, returns (Just True) when element is unique" $ do
    isUnique 'a' "foo bar"  `shouldBe` Just True

  it "isUnique , returns (Just False) when list has duplicate of element" $ do
    isUnique 'o' "foo bar" `shouldBe` Just False

  it "isUnique is a lazy function, 'undefined test'" $ do
    isUnique 'g' ['g','a','g', undefined ]
    `shouldBe`
      Just False

  it "isUnique is a lazy function, 'undefined test'. Generates exeption" $ do
    evaluate (isUnique 't' ['t', 'd', undefined] )
    `shouldThrow`
      errorCall "Prelude.undefined"

  it "isRepeated is a lazy function too, 'undefined test'" $ do
    isRepeated 'g' ['g','a','g', undefined ]
    `shouldBe`
      Just True

  it "isRepeated is reverse function to isUnique" $ do
    property $
      \ x xs
      -> isUnique (x :: Int) xs == fmap not (isRepeated x xs)

  it "isUnique should return (Just ANY) when element is exist in the list" $
    property $
    \ ( NonEmpty ls@(x:_) )
    -> isJust (isUnique (x :: Char) ls)

  context "isUnique should return (Just False) when at least two elements are exist in the list" $
    it "- It checks laziness as well" $
      property $
      \ ( NonEmpty ls@(x:_) )
      ->  isJust $ isUnique (x :: Char) (ls ++ [x, undefined])

  it "isUnique should return Nothing when element is absent in the list" $
    property $
    \ x xs
    -> notElem x xs
       ==> isNothing (isUnique (x :: Char) xs)
      
  it "isUnique and isRepeated should return Nothing when element is absent in the list" $
    property $
    \ x xs
    -> notElem x xs
       ==> isNothing (isUnique (x :: Char) xs)
       &&  isNothing (isRepeated x xs)
