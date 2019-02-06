import Lib

import Test.Hspec (Spec, hspec, describe, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "concatenator" $ do
    it "should concat" $ do
      (concatenator "a" "b") `shouldBe` "ab"
  describe "gameOfLife" $ do
    it "should says alive !!!" $ do
        gameOfLife `shouldBe` "Alive !!!"