import Lib

import Test.Hspec (Spec, hspec, describe, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Cell" $ do
    it "should display an alive cell" $ do
        show(Alive) `shouldBe` "O"
    it "should display an dead cell" $ do
        show(Dead) `shouldBe` "_"
  describe "Grid" $ do
    it "should display a grid of one dead Cell" $ do
        show(Line([Dead])) `shouldBe` "_"
    it "should display a grid of one alive Cell" $ do
      show(Line([Alive])) `shouldBe` "O"
    it "should display a grid of a line Cell" $ do
        show(Line([Dead, Dead, Alive])) `shouldBe` "__O"
    it "should display a grid of multiple lines of Cells" $ do
        show(Grid([Line([Dead, Dead, Alive]), Line([Alive, Dead, Alive])])) `shouldBe` "__O\nO_O"
  describe "Rules" $ do
    describe "Rule of Solitude" $ do
      it "living cell with no neighbors dies" $ do
        (nextGen Alive 0) `shouldBe` Dead
      it "living cell with one neighbors dies" $ do
        (nextGen Alive 1) `shouldBe` Dead
    describe "Rule of Overpopulation" $ do
      it "living cell with four neighbors dies" $ do
        (nextGen Alive 4) `shouldBe` Dead
      it "living cell with five neighbors dies" $ do
        (nextGen Alive 5) `shouldBe` Dead
      it "living cell with six neighbors dies" $ do
        (nextGen Alive 6) `shouldBe` Dead
      it "living cell with seven neighbors dies" $ do
        (nextGen Alive 7) `shouldBe` Dead
      it "living cell with eight neighbors dies" $ do
        (nextGen Alive 8) `shouldBe` Dead
    describe "Rule surviving cells" $ do
      it "living cell with two neighbors survive" $ do
        (nextGen Alive 2) `shouldBe` Alive
      it "living cell with three neighbors survive" $ do
        (nextGen Alive 3) `shouldBe` Alive
    describe "Rule newborn cells" $ do
      it "dead cell with three neighbors is alive" $ do
        (nextGen Dead 3) `shouldBe` Alive
      it "dead cell with two neighbors should stay dead" $ do
        (nextGen Dead 2) `shouldBe` Dead
      it "dead cell with four neighbors should stay dead" $ do
        (nextGen Dead 4) `shouldBe` Dead