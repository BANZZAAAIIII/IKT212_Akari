import org.scalatest.FunSuite

class test extends FunSuite {
  test("getNumPuzzles.PuzzleReaderWriter") {
    PuzzleReaderWriter.initRW("puzzleUnsolved.txt", "puzzleSolved.txt")
    assert(PuzzleReaderWriter.getNumPuzzles == 2)
  }
}

 /** A set of simple puzzles that the Akari solver can solve */
class SimplePuzzleTests extends FunSuite {
   import solver.filter_space
   val simple_board_X_44: Array[Array[Char]] = Array.ofDim[Char](4,4)
   simple_board_X_44(0) = "_ _ X *".toArray.filter(filter_space)
   simple_board_X_44(1) = "X _ _ X".toArray.filter(filter_space)
   simple_board_X_44(2) = "_ _ * _".toArray.filter(filter_space)
   simple_board_X_44(3) = "_ _ _ _".toArray.filter(filter_space)

   test("TestValidPlacementRow00.PuzzleSolver") {
     val x = 0
     val y = 0
     //assert(solver.check_row(simple_board_X_44(y), x))
   }

   test("TestValidPlacementRow10.PuzzleSolver") {
     val x = 0
     val y = 1
     //assert(solver.check_row(simple_board_X_44(y), x))
   }

   test("TestValidPlacementRow20.PuzzleSolver") {
     val x = 0
     val y = 2
     //assert(!solver.check_row(simple_board_X_44(y), x))
   }

   test("TestValidPlacementCol00.PuzzleSolver") {
     val x = 3
     val y = 0
     //assert(!solver.check_col(simple_board_X_44, x, y))
   }

   test("TestValidPlacementCol10.PuzzleSolver") {
     val x = 3
     val y = 1
     //assert(!solver.check_col(simple_board_X_44, x, y))
   }

   test("TestValidPlacementCol20.PuzzleSolver") {
     val x = 3
     val y = 2
     //assert(solver.check_col(simple_board_X_44, x, y))
   }
}
