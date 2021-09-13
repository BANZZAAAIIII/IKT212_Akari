import org.scalatest.FunSuite
import com.akari.types._
import solver.simple_solved_board

class test extends FunSuite {
  test("getNumPuzzles.PuzzleReaderWriter") {
    PuzzleReaderWriter.initRW("puzzleUnsolved.txt", "puzzleSolved.txt")
    assert(PuzzleReaderWriter.getNumPuzzles == 2)
  }
}

/** A set of simple puzzles that the Akari solver can solve */
class PlaceLightTests extends FunSuite {
  import solver.filter_space
  val test_board: List[List[Char]] = List(
    "_ 1 _".toList.filter(filter_space),
    "_ X _".toList.filter(filter_space),
    "_ 2 _".toList.filter(filter_space),
    "_ _ 2".toList.filter(filter_space),
    "_ _ _".toList.filter(filter_space),
    "_ _ _".toList.filter(filter_space)
  )

  test("TestValidPlacement.PuzzleSolver") {
    val x = 2
    val y = 0

    solver.place_light(test_board, x, y) match {
      case Some(b) => assert(b(y)(x) == Light)
      case None    => fail("This move should place a light")
    }
  }

  test("TestValidPlacement02.PuzzleSolver") {
    val x = 0
    val y = 2

    solver.place_light(test_board, x, y) match {
      case Some(b) => assert(b(y)(x) == Light)
      case None    => fail("This move should place a light")
    }
  }

  test("TestValidPlacementRow13.PuzzleSolver") {
    val x = 1
    val y = 3

    solver.place_light(test_board, x, y) match {
      case Some(b) => assert(b(y)(x) == Light)
      case None    => fail("This move should place a light")
    }
  }

  test("TestValidPlacementRow24.PuzzleSolver") {
    val x = 2
    val y = 4

    solver.place_light(test_board, x, y) match {
      case Some(b) => assert(b(y)(x) == Light)
      case None    => fail("This move should place a light")
    }
  }

  test("TestInvalidPlacement22.PuzzleSolver") {
    val x = 2
    val y = 2
    solver.place_light(test_board, x, y) match {
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

class CheckSolvedTests extends FunSuite {
  import solver.filter_space
  val simple_solved_board: List[List[Char]] = List(
    "_ * _ _".toList.filter(filter_space),
    "_ _ X _".toList.filter(filter_space),
    "_ _ * _".toList.filter(filter_space),
    "* _ X *".toList.filter(filter_space)
  )

  val simple_solved_board_numbers: List[List[Char]] = List(
    "1 * _ X".toList.filter(filter_space),
    "_ _ 1 _".toList.filter(filter_space),
    "_ _ * _".toList.filter(filter_space),
    "* _ 2 *".toList.filter(filter_space)
  )

  test("Testcheckifsolved.PuzzleSolver") {
    assert(solver.check_if_solved(simple_solved_board))
  }
  test("Testcheckifsolvednumbers.PuzzleSolver") {
    assert(solver.check_if_solved(simple_solved_board_numbers))
  }
}

