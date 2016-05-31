import           Test.Hspec (hspec, describe, it, shouldSatisfy, shouldBe)
import qualified Test.Hspec as H
import           Data.Maybe (Maybe, isJust, fromJust)
import qualified Data.Maybe as M
import           Data.Either (Either, isRight, isLeft)
import qualified Data.Either as E
import           Data.List ()
import qualified Data.List as L


import           Analyze
import qualified Analyze as A
import           Parse
import qualified Parse as P
import           Tokenize
import qualified Tokenize as T


main :: IO ()
main = hspec $ do
   let date = tokenizeTDate "Date: Tue, 4 May 2010 14:46:05 -0400" 
   let sect = tokenizeTSectionChange "12345678 : abcd: MEET_PRIMARY change from 002 to 003"
   let psect = parse $ map fromJust [date, sect]
   let psect' = parse $ map fromJust [sect, date]
   it "tokenize" $ do
      date `shouldSatisfy` isJust
      sect `shouldSatisfy` isJust
   it "parse" $ do
      psect `shouldSatisfy` isRight
      psect' `shouldSatisfy` isLeft
   it "analyze" $ do
      (length $ groupByStudent (concat $ concat $ sequence [psect, psect]))
         `shouldBe` 1
   
   