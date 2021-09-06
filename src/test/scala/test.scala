import org.scalatest.FunSuite
import Const._

class test extends FunSuite {
  test("getNumPuzzles.PuzzleReaderWriter") {
    PuzzleReaderWriter.initRW("puzzleUnsolved.txt", "puzzleSolved.txt")
    assert(PuzzleReaderWriter.getNumPuzzles == 2)
  }
}

/** A set of simple puzzles that the Akari solver can solve */
class SimplePuzzleTests extends FunSuite {
  import solver.filter_space
  val simple_board_X_44: List[List[Char]] = List(
    "_ _ X *".toList.filter(filter_space),
    "X _ _ X".toList.filter(filter_space),
    "_ _ * _".toList.filter(filter_space),
    "_ _ _ _".toList.filter(filter_space)
  )

  test("TestValidPlacement00.PuzzleSolver") {
    val x = 0
    val y = 0

    solver.place_light(simple_board_X_44, x, y) match {
      case Some(b) => assert(b(y)(x) == Light)
      case None    => fail("This move should place a light")
    }
  }

  test("TestValidPlacement10.PuzzleSolver") {
    val x = 1
    val y = 0

    solver.place_light(simple_board_X_44, x, y) match {
      case Some(b) => assert(b(y)(x) == Light)
      case None    => fail("This move should place a light")
    }
  }

  test("TestValidPlacementRow13.PuzzleSolver") {
    val x = 1
    val y = 3

    solver.place_light(simple_board_X_44, x, y) match {
      case Some(b) => assert(b(y)(x) == Light)
      case None    => fail("This move should place a light")
    }
  }

  test("TestInvalidPlacement30.PuzzleSolver") {
    val x = 3
    val y = 0
    solver.place_light(simple_board_X_44, x, y) match {
      case Some(_) => fail("This move shouldn't be legal")
      case None    => succeed
    }
  }
}

class CheckListTests extends FunSuite {
  import solver.filter_space
  val simple_board: List[List[Char]] = List(
    "_ _ _ _".toList.filter(filter_space),
    "_ _ X _".toList.filter(filter_space),
    "_ _ * _".toList.filter(filter_space),
    "_ _ X *".toList.filter(filter_space)
  )

  test("TestCheckListEmpty.PuzzleSolver") {
    assert(solver.check_list(simple_board.head, simple_board.head.indices))
  }

  test("TestCheckListX.PuzzleSolver") {
    assert(solver.check_list(simple_board(1), simple_board(1).indices))
  }

  test("TestCheckList*.PuzzleSolver") {
    assert(!solver.check_list(simple_board(2), simple_board(2).indices))
  }

  test("TestCheckListX*.PuzzleSolver") {
    assert(solver.check_list(simple_board(3), simple_board(3).indices))
  }
}
