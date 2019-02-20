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
  
  describe "Find Neighbors" $ do
    it "Should find neighbors on one line" $ do
      let grid = Grid [(Line [Alive, Dead, Alive, Dead, Dead])]
        in (neighbors grid 0 2) `shouldBe` [Dead, Dead]
    it "Should find neighbors on one line when at the beginning of the line" $ do
      let grid = Grid [(Line [Alive, Dead, Alive, Dead, Dead])]
        in (neighbors grid 0 0) `shouldBe` [Dead]
    it "Should find neighbors on one line when at the end of the line" $ do
      let grid = Grid [(Line [Alive, Dead, Alive, Dead, Dead])]
        in (neighbors grid 0 4) `shouldBe` [Dead]
    it "Should find neighbors in the  upper line" $ do
      let grid = Grid [(Line [Alive, Dead, Alive]),(Line [Alive, Dead, Alive])]
        in (neighbors grid 1 1) `shouldBe` [Alive, Dead, Alive, Alive, Alive]
    it "Should find neighbors when no cells in the upper left" $ do
      let grid = Grid [(Line [Alive, Dead, Alive]),(Line [Alive, Dead, Alive])]
        in (neighbors grid 1 0) `shouldBe` [Alive, Dead, Dead]
    it "Should find neighbors when no cells in the upper right" $ do
      let grid = Grid [(Line [Alive, Dead, Alive]),(Line [Alive, Dead, Alive])]
        in (neighbors grid 1 2) `shouldBe` [Dead, Alive, Dead]
    it "Should find neighbors in the bottom line" $ do
      let grid = Grid [(Line [Alive, Dead, Alive]),(Line [Alive, Dead, Alive])]
        in (neighbors grid 0 1) `shouldBe` [Alive, Alive, Alive, Dead, Alive]
    it "Should find neighbors when no cells in the lower left" $ do
      let grid = Grid [(Line [Alive, Dead, Alive]),(Line [Alive, Dead, Alive])]
        in (neighbors grid 0 0) `shouldBe` [Dead, Alive, Dead]
    it "Should find neighbors when no cells in the lower right" $ do
      let grid = Grid [(Line [Alive, Dead, Alive]),(Line [Alive, Dead, Alive])]
        in (neighbors grid 0 2) `shouldBe` [Dead, Dead, Alive]

  describe "Count alive Neighbors" $ do
    describe "a single line Grid" $ do
      it "should have no neighbors" $ do
        let grid = (Grid [(Line [Dead, Alive, Dead]) ]) 
          in (countNeighbors grid 0 1) `shouldBe` 0
      it "should have one neighbor" $ do
        let grid = (Grid [(Line [Dead, Dead, Alive, Alive, Dead, Alive]) ]) 
          in (countNeighbors grid 0 3) `shouldBe` 1
        
  describe "left" $ do
    it "should return the left cell when available" $ do
      (left [1, 2] 1) `shouldBe` (Just 1)
    it "should return Nothing when not left cell" $ do
      (left [1, 2] 0) `shouldBe` (Nothing)

  describe "right" $ do
    it "should return the right cell when available" $ do
      (right [1, 2] 0) `shouldBe` (Just 2)
    it "should return Nothing when no right cell" $ do
      (right [1, 2] 1) `shouldBe` (Nothing)
  describe "isAlive" $ do
    it  "should return true when is Alive" $ do
      (isAlive Alive) `shouldBe` True
    it  "should return false when is Dead" $ do
      (isAlive Dead) `shouldBe` False


  describe "Evolving" $ do
    describe "Next Generation computation" $ do
      it "dead cell with three neighbors is alive" $ do
        nextGen(Grid([Line([Dead, Alive, Dead]), Line([Dead, Alive, Dead]), Line([Dead, Alive, Dead])])) `shouldBe` Grid([Line([Dead, Dead, Dead]), Line([Alive, Alive, Alive]), Line([Dead, Dead, Dead])])