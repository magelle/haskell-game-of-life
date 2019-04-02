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
      showLine([Dead]) `shouldBe` "_"
    it "should display a grid of one alive Cell" $ do
      showLine([Alive]) `shouldBe` "O"
    it "should display a grid of a line Cell" $ do
      showLine([Dead, Dead, Alive]) `shouldBe` "__O"
    it "should display a grid of multiple lines of Cells" $ do
        showGrid([[Dead, Dead, Alive], [Alive, Dead, Alive]]) `shouldBe` "__O\nO_O"


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
  
  describe "Find Neighbors" $ do
    it "Should find neighbors on one line" $ do
      let grid = [[Alive, Dead, Alive, Dead, Dead]]
        in (neighbors grid 0 2) `shouldBe` [Dead, Dead]
    it "Should find neighbors on one line when at the beginning of the line" $ do
      let grid = [[Alive, Dead, Alive, Dead, Dead]]
        in (neighbors grid 0 0) `shouldBe` [Dead]
    it "Should find neighbors on one line when at the end of the line" $ do
      let grid = [[Alive, Dead, Alive, Dead, Dead]]
        in (neighbors grid 0 4) `shouldBe` [Dead]
    it "Should find neighbors in the  upper line" $ do
      let grid = [[Alive, Dead, Alive],[Alive, Dead, Alive]]
        in (neighbors grid 1 1) `shouldBe` [Alive, Dead, Alive, Alive, Alive]
    it "Should find neighbors when no cells in the upper left" $ do
      let grid = [[Alive, Dead, Alive],[Alive, Dead, Alive]]
        in (neighbors grid 1 0) `shouldBe` [Alive, Dead, Dead]
    it "Should find neighbors when no cells in the upper right" $ do
      let grid = [[Alive, Dead, Alive],[Alive, Dead, Alive]]
        in (neighbors grid 1 2) `shouldBe` [Dead, Alive, Dead]
    it "Should find neighbors in the bottom line" $ do
      let grid = [[Alive, Dead, Alive],[Alive, Dead, Alive]]
        in (neighbors grid 0 1) `shouldBe` [Alive, Alive, Alive, Dead, Alive]
    it "Should find neighbors when no cells in the lower left" $ do
      let grid = [[Alive, Dead, Alive],[Alive, Dead, Alive]]
        in (neighbors grid 0 0) `shouldBe` [Dead, Alive, Dead]
    it "Should find neighbors when no cells in the lower right" $ do
      let grid = [[Alive, Dead, Alive],[Alive, Dead, Alive]]
        in (neighbors grid 0 2) `shouldBe` [Dead, Dead, Alive]

  describe "Count alive Neighbors" $ do
    describe "a single line Grid" $ do
      it "should have no neighbors" $ do
        let grid = [[Dead, Alive, Dead]]
          in (countNeighbors grid 0 1) `shouldBe` 0
      it "should have one neighbor" $ do
        let grid = [[Dead, Dead, Alive, Alive, Dead, Alive]]
          in (countNeighbors grid 0 3) `shouldBe` 1

  describe "Evolving" $ do
    describe "Next Generation computation" $ do
      it "dead cell with three neighbors is alive" $ do
        nextGen([[Dead, Alive, Dead], [Dead, Alive, Dead], [Dead, Alive, Dead]]) `shouldBe` [[Dead, Dead, Dead], [Alive, Alive, Alive], [Dead, Dead, Dead]]

  describe "neighborsCoord" $ do
    it "should return all direct neighbors" $ do
      neighborsCoord (2, 3) `shouldBe` [(1,2),(1,3),(1,4),(2,2),(2,4),(3,2),(3,3),(3,4)]
  describe "removeOutside" $ do
    it "should remove all coord outside a grid" $ do
      removeOutside [[1]] [(-1, 0), (0, -1), (0, 0), (0, 1), (1, 0)] `shouldBe` [(0, 0)]