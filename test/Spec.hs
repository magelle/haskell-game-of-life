
import Test.Hspec (Spec, hspec, describe, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "player scoring" $ do
    it "should save score of player" $ do
      ("a" ++ "b") `shouldBe` "ab"