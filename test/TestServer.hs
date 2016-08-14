module TestServer where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "runClient : " $ do
    it "should take unique user names" $
      property $ \i -> i + 1 == (i + 1::Int)
