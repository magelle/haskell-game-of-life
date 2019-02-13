import Lib

import Test.Hspec (Spec, hspec, describe, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Take From" $ do
    it "should take the array from start" $ do
      (takeFrom 1 2 [1, 2, 3, 4]) `shouldBe` [2, 3]

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
        (nextCellState Alive 0) `shouldBe` Dead
      it "living cell with one neighbors dies" $ do
        (nextCellState Alive 1) `shouldBe` Dead
    describe "Rule of Overpopulation" $ do
      it "living cell with four neighbors dies" $ do
        (nextCellState Alive 4) `shouldBe` Dead
      it "living cell with five neighbors dies" $ do
        (nextCellState Alive 5) `shouldBe` Dead
      it "living cell with six neighbors dies" $ do
        (nextCellState Alive 6) `shouldBe` Dead
      it "living cell with seven neighbors dies" $ do
        (nextCellState Alive 7) `shouldBe` Dead
      it "living cell with eight neighbors dies" $ do
        (nextCellState Alive 8) `shouldBe` Dead
    describe "Rule surviving cells" $ do
      it "living cell with two neighbors survive" $ do
        (nextCellState Alive 2) `shouldBe` Alive
      it "living cell with three neighbors survive" $ do
        (nextCellState Alive 3) `shouldBe` Alive
    describe "Rule newborn cells" $ do
      it "dead cell with three neighbors is alive" $ do
        (nextCellState Dead 3) `shouldBe` Alive
      it "dead cell with two neighbors should stay dead" $ do
        (nextCellState Dead 2) `shouldBe` Dead
      it "dead cell with four neighbors should stay dead" $ do
        (nextCellState Dead 4) `shouldBe` Dead

  describe "Evolving" $ do
    describe "Next Generation computation" $ do
      it "dead cell with three neighbors is alive" $ do
        nextGen(Grid([Line([Dead, Alive, Dead]), Line([Dead, Alive, Dead]), Line([Dead, Alive, Dead])])) `shouldBe` Grid([Line([Dead, Dead, Dead]), Line([Alive, Alive, Alive]), Line([Dead, Dead, Dead])])
  
  describe "Find Neighbors" $ do
    it "Should remove target" $ do
      let line = Line [Alive, Dead, Alive, Dead, Dead]
        in (neighbors 2 line) `shouldBe` [Dead, Dead]

  describe "Count alive Neighbors" $ do
    describe "a single line Grid" $ do
      it "should have no neighbors" $ do
        let grid = (Grid [(Line [Dead, Alive, Dead]) ]) 
          in (countNeighbors grid 0 1) `shouldBe` 0
      it "should have one neighbor" $ do
        let grid = (Grid [(Line [Dead, Dead, Alive, Alive, Dead, Alive]) ]) 
          in (countNeighbors grid 0 3) `shouldBe` 1
        
 