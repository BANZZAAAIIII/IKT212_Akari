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
    var pos = new Position(2, 0)

    solver.place_light(test_board, pos) match {
      case Some(b) => assert(b(pos.row)(pos.col) == Light)
      case None    => fail("This move should place a light")
    }
  }

  test("TestValidPlacement02.PuzzleSolver") {
    var pos = new Position(0, 2)

    solver.place_light(test_board, pos) match {
      case Some(b) => assert(b(pos.row)(pos.col) == Light)
      case None    => fail("This move should place a light")
    }
  }

  test("TestValidPlacementRow13.PuzzleSolver") {
    var pos = new Position(1, 3)

    solver.place_light(test_board, pos) match {
      case Some(b) => assert(b(pos.row)(pos.col) == Light)
      case None    => fail("This move should place a light")
    }
  }

  test("TestValidPlacementRow24.PuzzleSolver") {
    var pos = new Position(2, 4)

    solver.place_light(test_board, pos) match {
      case Some(b) => assert(b(pos.row)(pos.col) == Light)
      case None    => fail("This move should place a light")
    }
  }

  test("TestInvalidPlacement22.PuzzleSolver") {
    var pos = new Position(2, 2)

    solver.place_light(test_board, pos) match {
      case Some(_) => fail("This move shouldn't be legal")
      case None    => succeed
    }
  }
}

class CheckAdjacentTests extends FunSuite {
  import solver.filter_space
  import solver.check_tile_if_num
  import solver.check_tile_for_light
  val test_board: List[List[Char]] = List(
    "_ 1 _".toList.filter(filter_space),
    "_ X _".toList.filter(filter_space),
    "_ 2 _".toList.filter(filter_space),
    "_ _ _".toList.filter(filter_space),
    "_ _ _".toList.filter(filter_space),
    "1 _ _".toList.filter(filter_space)
  )

  test("TestAdjacentListEmpty.PuzzleSolver") {
    val pos = new Position(1,2)

    Option(solver.check_adjacent(test_board, pos, check_tile_if_num)) match {
      case Some(b) => assert(b.isEmpty)
      case None => fail
    }
  }
  test("TestAdjacentTop.PuzzleSolver") {
    val pos = new Position(3,1)
    Option(solver.check_adjacent(test_board, pos, check_tile_if_num)) match {
      case Some(b) => assert(b(0) == Position(2, 1))
      case None => fail
    }
  }
  test("TestAdjacentBottom.PuzzleSolver") {
    val pos = new Position(4,0)
    Option(solver.check_adjacent(test_board, pos, check_tile_if_num)) match {
      case Some(b) => assert(b(0) == Position(5, 0))
      case None => fail
    }
  }
  test("TestAdjacentRight.PuzzleSolver") {
    val pos = new Position(0,0)
    Option(solver.check_adjacent(test_board, pos, check_tile_if_num)) match {
    case Some(b) => assert(b(0) == Position(0, 1))
    case None => fail
    }
  }

  test("TestAdjacentLeft.PuzzleSolver") {
    val pos = new Position(0,2)
    Option(solver.check_adjacent(test_board, pos, check_tile_if_num)) match {
    case Some(b) => assert(b(0) == Position(0, 1))
    case None => fail
    }
  }

  test("TestAdjacentWall.PuzzleSolver") {
    val pos = new Position(2,2)
    Option(solver.check_adjacent(test_board, pos, check_tile_for_light)) match {
    case Some(b) => assert(b.isEmpty)
    case None => fail
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
  val simple_solved_board: Matrix = List(
    "_ * _ _".toList.filter(filter_space),
    "_ _ X _".toList.filter(filter_space),
    "_ _ * _".toList.filter(filter_space),
    "* _ X *".toList.filter(filter_space)
  )

  val simple_solved_board_numbers: Matrix = List(
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

