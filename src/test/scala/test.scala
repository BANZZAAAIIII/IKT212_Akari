import org.scalatest.FunSuite
import com.akari.types._

import solver.{check_placement, check_tile_if_num, check_tile_if_light}
import utility.Util.filter_space


class PlaceLightTests extends FunSuite {
  val test_board: Matrix = List(
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
    var pos = new Position(3, 1)

    solver.place_light(test_board, pos) match {
      case Some(b) => assert(b(pos.row)(pos.col) == Light)
      case None    => fail("This move should place a light")
    }
  }

  test("TestValidPlacementRow24.PuzzleSolver") {
    var pos = new Position(4, 2)

    solver.place_light(test_board, pos) match {
      case Some(b) => assert(b(pos.row)(pos.col) == Light)
      case None    => fail("This move should place a light")
    }
  }

  test("TestInvalidPlacement22.PuzzleSolver") {
    var pos = new Position(2, 1)

    solver.place_light(test_board, pos) match {
      case Some(_) => fail("This move shouldn't be legal")
      case None    => succeed
    }
  }
}

class PlaceLightDeterministicTest extends FunSuite {
  val id_7x7_a2Bj0fBc3cBf1j10a: Matrix = List(
    "_ 2 X _ _ _ _".toList.filter(filter_space),
    "_ _ _ _ _ _ 0".toList.filter(filter_space),
    "_ _ _ _ _ _ X".toList.filter(filter_space),
    "_ _ _ 3 _ _ _".toList.filter(filter_space),
    "X _ _ _ _ _ _".toList.filter(filter_space),
    "1 _ _ _ _ _ _".toList.filter(filter_space),
    "_ _ _ _ 1 0 _".toList.filter(filter_space)
  )

  val id_7x7_a2Bj0fBc3cBf1j10a_solved: Matrix = List(
    "* 2 X _ _ _ _".toList.filter(filter_space),
    "_ * _ _ _ _ 0".toList.filter(filter_space),
    "_ _ _ * _ _ X".toList.filter(filter_space),
    "_ _ * 3 _ _ _".toList.filter(filter_space),
    "X _ _ * _ _ _".toList.filter(filter_space),
    "1 _ _ _ * _ _".toList.filter(filter_space),
    "* _ _ _ 1 0 _".toList.filter(filter_space)
  )

  val id_4x8_3AaBa3d1a3c0nBb: Matrix = List(
    "_ X _ 3 _ _ _ _ ".toList.filter(filter_space),
    "1 _ 3 _ _ _ 0 _ ".toList.filter(filter_space),
    "_ _ _ _ _ _ _ _ ".toList.filter(filter_space),
    "_ _ _ _ _ X _ _ ".toList.filter(filter_space)
  )

  val id_4x8_3AaBa3d1a3c0nBb_solved: Matrix = List(
    "_ X * 3 * _ _ _ ".toList.filter(filter_space),
    "1 _ 3 * _ _ 0 _ ".toList.filter(filter_space),
    "_ _ _ _ _ _ _ _ ".toList.filter(filter_space),
    "_ _ _ _ _ X _ _ ".toList.filter(filter_space)
  )



  test("TestDeterministicLightPlacement.PuzzleSolver") {
    var pos = new Position(2, 0)

    assert(solver.trivial_solver(id_4x8_3AaBa3d1a3c0nBb) == id_4x8_3AaBa3d1a3c0nBb_solved)
    assert(solver.trivial_solver(id_7x7_a2Bj0fBc3cBf1j10a) == id_7x7_a2Bj0fBc3cBf1j10a_solved)
  }

}

class CheckAdjacentValidPlacementTest extends FunSuite {
  def filter_space(c: Char): Boolean = c != ' '
  val test_board: Matrix = List(
    "_ 1 _ _".toList.filter(filter_space),
    "_ X X 0".toList.filter(filter_space),
    "_ 2 _ _".toList.filter(filter_space),
    "_ _ 3 X".toList.filter(filter_space),
    "_ _ _ _".toList.filter(filter_space),
    "_ _ X _".toList.filter(filter_space)
  )

  test("TestAdjacentList3.PuzzleSolver") {
    val pos = Position(3,2)

    val nr_of_adjacent = solver.check_adjacent(test_board, pos, check_placement).length
    assert(nr_of_adjacent == 3)
  }
}

class CheckAdjacentTests extends FunSuite {
  val test_board: Matrix = List(
    "_ 1 _".toList.filter(filter_space),
    "_ X _".toList.filter(filter_space),
    "_ 2 _".toList.filter(filter_space),
    "_ _ _".toList.filter(filter_space),
    "_ _ _".toList.filter(filter_space),
    "1 _ _".toList.filter(filter_space)
  )

  test("TestAdjacentListEmpty.PuzzleSolver") {
    val pos = new Position(1,2)

    Option(solver.check_adjacent(test_board, pos, solver.char_to_board_pos(_: Matrix, _: Position, check_tile_if_num))) match {
      case Some(b) => assert(b.isEmpty)
      case None => fail
    }
  }
  test("TestAdjacentTop.PuzzleSolver") {
    val pos = new Position(3,1)
    Option(solver.check_adjacent(test_board, pos, solver.char_to_board_pos(_: Matrix, _: Position, check_tile_if_num))) match {
      case Some(b) => assert(b(0) == Position(2, 1))
      case None => fail
    }
  }
  test("TestAdjacentBottom.PuzzleSolver") {
    val pos = new Position(4,0)
    Option(solver.check_adjacent(test_board, pos, solver.char_to_board_pos(_: Matrix, _: Position, check_tile_if_num))) match {
      case Some(b) => assert(b(0) == Position(5, 0))
      case None => fail
    }
  }
  test("TestAdjacentRight.PuzzleSolver") {
    val pos = new Position(0,0)
    Option(solver.check_adjacent(test_board, pos, solver.char_to_board_pos(_: Matrix, _: Position, check_tile_if_num))) match {
    case Some(b) => assert(b(0) == Position(0, 1))
    case None => fail
    }
  }

  test("TestAdjacentLeft.PuzzleSolver") {
    val pos = new Position(0,2)
    Option(solver.check_adjacent(test_board, pos, solver.char_to_board_pos(_: Matrix, _: Position, check_tile_if_num))) match {
    case Some(b) => assert(b(0) == Position(0, 1))
    case None => fail
    }
  }

  test("TestAdjacentWall.PuzzleSolver") {
    val pos = new Position(2,2)
    Option(solver.check_adjacent(test_board, pos, solver.char_to_board_pos(_: Matrix, _: Position, check_tile_if_light))) match {
    case Some(b) => assert(b.isEmpty)
    case None => fail
    }
  }
}

class CheckListTests extends FunSuite {
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

  val solved_board7x7: Matrix = List(
    "* _ _ _ _ _ _".toList.filter(filter_space),
    "2 _ _ * _ _ _".toList.filter(filter_space),
    "* 1 _ 2 0 _ *".toList.filter(filter_space),
    "X _ _ * _ _ _".toList.filter(filter_space),
    "X _ * _ _ _ _".toList.filter(filter_space),
    "* _ _ _ _ 0 _".toList.filter(filter_space),
    "2 * X _ * _ _".toList.filter(filter_space)
  )

  test("Testcheckifsolved.PuzzleSolver") {
    assert(solver.check_if_solved(simple_solved_board))
    assert(solver.check_if_solved(simple_solved_board_numbers))
    assert(solver.check_if_solved(solved_board7x7))
  }
}

class CheckFilterEmptyTests extends FunSuite {
  val test_board: List[List[Char]] = List(
    "* 1 _".toList.filter(filter_space),
    "_ X _".toList.filter(filter_space),
    "_ 2 _".toList.filter(filter_space),
    "_ _ 2".toList.filter(filter_space),
    "X _ _".toList.filter(filter_space), // TODO: Add test to see if empty spaces are removed when pos=(4,1) is used
    "_ _ _".toList.filter(filter_space)
  )

  val candidates: List[Position] = List(Position(0,0), Position(0,2), Position(1,0), Position(1,2), Position(2,0), Position(2,2), Position(3,0), Position(3,1), Position(4,1), Position(4,2), Position(5,0), Position(5,1), Position(5,2))
  test("TestCheckIfFilteredOne.PuzzleSolver") {
    val filtered_candidates: List[Position] = List(Position(0,2), Position(1,2), Position(2,2), Position(3,1), Position(4,1), Position(4,2), Position(5,0), Position(5,1), Position(5,2))
    val pos: Position = new Position(0,0)
    solver.filter_litup(test_board, candidates, pos) match {
      case can:List[Position] => assert(can == filtered_candidates)
      case _ => fail("Something went wrong")
    }
  }

  test("TestCheckIfFilteredTwo.PuzzleSolver") {
    val filtered_candidates: List[Position] = List(Position(0,0), Position(0,2), Position(1,0), Position(1,2), Position(2,0), Position(2,2), Position(3,0), Position(5,0), Position(5,2))
    val pos: Position = new Position(4,1)
    solver.filter_litup(test_board, candidates, pos) match {
      case can:List[Position] => assert(can == filtered_candidates)
      case _ => fail("Something went wrong")
    }
  }


}

class CheckWallBetweenTiles extends FunSuite {
    val test_board: List[List[Char]] = List(
      "* 1 _".toList.filter(filter_space),
      "_ X _".toList.filter(filter_space),
      "_ 2 _".toList.filter(filter_space),
      "_ _ 2".toList.filter(filter_space),
      "X * _".toList.filter(filter_space), 
      "_ _ _".toList.filter(filter_space)
    )

    test("TestCheckIfWallBetweenOnRowOrColumns.PuzzleSolver") {
      assert(solver.check_wall_between_tiles(test_board, new Position(0, 2), new Position(0,0)) == true)
      assert(solver.check_wall_between_tiles(test_board, new Position(0, 0), new Position(0,2)) == true)
      assert(solver.check_wall_between_tiles(test_board, new Position(1, 0), new Position(5,0)) == true)
      assert(solver.check_wall_between_tiles(test_board, new Position(4, 2), new Position(2,2)) == true)
    }

    test("TestCheckIfNoWallBetweenOnRowOrColumns.PuzzleSolver") {
      assert(solver.check_wall_between_tiles(test_board, new Position(3, 0), new Position(3,1)) == false)
      assert(solver.check_wall_between_tiles(test_board, new Position(0, 0), new Position(2,0)) == false)
    }

    test("TestCheckIfTileAndLightAreDifferentRowsAndColumns.PuzzleSolver") {
      assert(solver.check_wall_between_tiles(test_board, new Position(1, 0), new Position(3,0)) == false)
      assert(solver.check_wall_between_tiles(test_board, new Position(1, 0), new Position(0,2)) == false)
    }
}

class RemoveFilledWalls extends FunSuite {
  val test_board: Matrix = List(
    "X X _ X X".toList.filter(filter_space),
    "X _ 0 _ X".toList.filter(filter_space),
    "X X _ X X".toList.filter(filter_space)
  )
  val test_board2: Matrix = List(
    "0 _ X X".toList.filter(filter_space),
    "_ X X _".toList.filter(filter_space),
    "X X _ 0".toList.filter(filter_space)
  )
  val test_board3: Matrix = List(
    "X _ 0 _".toList.filter(filter_space),
    "X _ _ _".toList.filter(filter_space),
    "* 2 * 1".toList.filter(filter_space)
  )
  val test_board4: Matrix = List(
    "_ _ 0 _".toList.filter(filter_space),
    "_ _ _ _".toList.filter(filter_space),
    "* 2 * 1".toList.filter(filter_space)
  )
  test("TestRemoveWalledCandidatesNoCandidatesRemaining.PuzzleSolver") {
    assert(solver.remove_walled_candidates(test_board, solver.find_tiles(test_board, solver.check_tile_if_Empty)).isEmpty)
    assert(solver.remove_walled_candidates(test_board2, solver.find_tiles(test_board2, solver.check_tile_if_Empty)).isEmpty)
    assert(solver.remove_walled_candidates(test_board3, solver.find_tiles(test_board3, solver.check_tile_if_Empty)).isEmpty)
  }
  test("TestRemoveWalledCandidatesCandidatesRemaining.PuzzleSolver") {
    val candidates = solver.find_tiles(test_board4, solver.check_tile_if_Empty)
    val solutionCandidates: List[Position] = List(Position(0,0), Position(1,0))
    val removeWalledCandidates = solver.remove_walled_candidates(test_board4, candidates)
    assert(removeWalledCandidates == solutionCandidates)
  }
}

class RemoveLitupCandidates extends FunSuite {
  test("TestRemoveLitupCandidatesNoLights.PuzzleSolver") {
    val test_board: Matrix = List(
    "_ _ 0 _".toList.filter(filter_space),
    "_ _ _ _".toList.filter(filter_space),
    "_ 2 _ 1".toList.filter(filter_space))
    val candidates = solver.find_tiles(test_board, solver.check_tile_if_Empty)
    val lights = solver.find_tiles(test_board, solver.check_tile_if_light)
    assert(solver.remove_litup_candidates(test_board, candidates, lights) == candidates)
  }
  test("TestRemoveLitupCandidatesWithLights.PuzzleSolver") {
    val test_board: Matrix = List(
    "* _ _ _".toList.filter(filter_space),
    "_ _ _ *".toList.filter(filter_space))
    val candidates = solver.find_tiles(test_board, solver.check_tile_if_Empty)
    val lights = solver.find_tiles(test_board, solver.check_tile_if_light)
    assert(solver.remove_litup_candidates(test_board, candidates, lights).isEmpty)
  }
  test("TestRemoveLitupCandidatesWithLightsAndEmptyTiles.PuzzleSolver") {
    val test_board: Matrix = List(
    "* _ _ _".toList.filter(filter_space),
    "_ _ _ *".toList.filter(filter_space),
    "_ _ _ _".toList.filter(filter_space))
    val candidates = solver.find_tiles(test_board, solver.check_tile_if_Empty)
    val lights = solver.find_tiles(test_board, solver.check_tile_if_light)
    val solution = List(Position(2,1), Position(2,2))
    assert(solver.remove_litup_candidates(test_board, candidates, lights) == solution)
  }
}

class TestScalaStuff extends FunSuite {
  val test_board: List[List[Char]] = List(
    "* 1 _".toList.filter(filter_space),
    "_ X _".toList.filter(filter_space),
    "_ 2 _".toList.filter(filter_space),
    "_ _ 2".toList.filter(filter_space),
    "X _ _".toList.filter(filter_space), 
    "_ _ _".toList.filter(filter_space)
  )

}

