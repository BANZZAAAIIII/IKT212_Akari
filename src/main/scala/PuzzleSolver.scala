
import PuzzleReaderWriter.{getPuzzle, putSolution}
import com.akari.types._
import stopwatch.Stopwatch
import utility.Boards

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object PuzzleSolver extends App{
  // Solver function
  def solve(puzzle: Puzzle): Puzzle = {
    val timer = new Stopwatch() // Use to take realtime

    println("Solving puzzle")
    puzzle.printBoard()

    timer.start()
    val board = solver.place_light_deterministic(puzzle.board)
    val solved_board: Option[Matrix] = solver.backtracking(board, solver.get_candidates(board))
    timer.stop()

    val new_puzzle = new Puzzle(puzzle.sizeRow, puzzle.sizeCol, solved_board, puzzle.board)

    new_puzzle.printSolvedBoard()
    println("Promising: " + solver.promising + "\n" + "Visited: " + solver.visited)
    println("Time: " + timer.stop().getElapsedTime() + "ms")

    return new_puzzle
  }

  if (args.nonEmpty)
    putSolution(args(1), solve(getPuzzle(args(0))))
  else
    println("No arguments given")
//    solve(new Puzzle(4, 5, None, Boards.id_7x7_c3mBc2a2a2a0c2m0c))

}

object solver extends App {
  // Globals vars used to check visited and promising nodes when using backtracking
  var visited = 0
  var promising = 0


  /** Returns true when given a empty tile */
  def check_tile_if_Empty(c: Char): Boolean = if (c == Empty) true else false

  /** Returns true when given a light */
  def check_tile_if_light(c: Char): Boolean = if (c == Light) true else false

  /** Returns true when given a Zero wall */
  def check_tile_if_zero(c: Char): Boolean = if (c == Zero) true else false

  /** Returns true if the tile has any number/constraint */
  def check_tile_if_num(c: Char): Boolean = c match {
    case Zero | One | Two | Three | Four  => true
    case _ => false
  }

  def check_tile_if_wall(c: Char): Boolean = c match {
    case Zero | One | Two | Three | Four  => true // TODO: Check if we can use check_tile_if_num here instead of duplicate
    case Wall => true
    case _ => false
  }

  /** Used to convert a function with signature Char => Boolean to (Matrix, Position) => Boolean */
  def char_to_board_pos(board: Matrix, pos: Position, condition: Char => Boolean): Boolean = {
    condition(board(pos.row)(pos.col))
  }

  /** Checks a list if there is a light in it.
   *  Takes a Range to iterate over the list */
  def check_list(board:List[Char], range: Range): Boolean = {
    range.foreach(index => {
      val tile = board(index)
      if (!check_tile_if_Empty(tile)) {
        return !check_tile_if_light(tile)
      }
    })
    return true
  }

  /** Checks all sides of a x, y pos for a light.
   *  Returns true of there is no light on the same row or col blocking */
  def check_placement(board: Matrix, pos: Position): Boolean = {
    if (check_tile_if_wall(board(pos.row)(pos.col))) return false

    // Checks for a light in a positive direction for x, y pos
    // if light is found then returns early with false to avoid checking in negative direction
    lazy val positiveXDir: Boolean = check_list(board(pos.row), pos.col until board.head.length)
    lazy val positiveYDir: Boolean = check_list(for(a <- board) yield a(pos.col), pos.row until board.length)
    if (!(positiveXDir && positiveYDir)) return false

    // Checks for a light in a negative direction for x, y pos
    lazy val negativeXDir: Boolean = check_list(board(pos.row), (0 until pos.col).reverse)
    lazy val negativeYDir: Boolean = check_list(for(a <- board) yield a(pos.col), (0 until pos.row).reverse)
    return negativeXDir && negativeYDir
  }

  /** Places a light on a x, y position if the tile is valid.
   *  Returns None if placement is illegal or a new board if not */
  def place_light(board: Matrix, pos: Position): Option[Matrix] = {
    if (!check_tile_if_Empty(board(pos.row)(pos.col))) return None

    check_adjacent(board, pos, char_to_board_pos(_:Matrix, _:Position, check_tile_if_num)).foreach( num => {
      val nr_of_light = get_number_of_lights_around_number(board, num)
      if (nr_of_light >= board(num.row)(num.col).asDigit)
        return None
    })

    if (check_placement(board, pos))
      return Option(board.updated(pos.row, board(pos.row).updated(pos.col, Light)))
    else
      return None
  }

  /** Gets all position adjacent to given pos that is inside the board*/
  def get_adjacent(board: Matrix, pos: Position): List[Position] = {
    val positions = new ListBuffer[Position]()
    // TODO: Try to improve this by making it more functional
    // Check Left cell
    if (!(pos.col - 1 < 0))
      positions += Position(pos.row, pos.col - 1)

    // Check Right Cell
    if (!(pos.col + 1 > board.head.length - 1))
      positions += Position(pos.row, pos.col + 1)

    // Check Bottom Cell
    if (!(pos.row - 1 < 0))
      positions += Position(pos.row - 1, pos.col)

    // Check Top Cell
    if (!(pos.row + 1 > board.length - 1))
      positions += Position(pos.row + 1, pos.col)

    positions.toList
  }

  /** Checks all valid positions adjacent to x, y pos on board.
   *  Takes a functions, condition: Char => Boolean, to check adjacent squares.
   *  Returns all x, y positions where condition is true */
  def check_adjacent(board: Matrix, pos: Position, condition: (Matrix, Position) => Boolean): List[Position] = {
    val positions = new ListBuffer[Position]()

    get_adjacent(board, pos).foreach(adjacent =>
      if (condition(board, adjacent)) positions += adjacent
    )
    return positions.toList
  }

  /** Gets number of lights adjacent to a square */
  def get_number_of_lights_around_number(board: Matrix, pos: Position): Int =
    check_adjacent(board, pos, char_to_board_pos(_: Matrix, _: Position, check_tile_if_light)).length

  /** Checks the puzzle is solved
   *  Doesn't check if lights are placed incorrectly */
  def check_if_solved(board: Matrix): Boolean = {
    // Finds all number tiles and checks if it has enough lights
    if (find_tiles(board, check_tile_if_num)
        .forall(wall => get_number_of_lights_around_number(board, wall) == board(wall.row)(wall.col).asDigit)) {
      // Checks that all empty tiles on the board is litup
      if (remove_litup_candidates(
        board,
        find_tiles(board, check_tile_if_Empty),
        find_tiles(board, check_tile_if_light)
      ).isEmpty)
        return true
      else
        return false
    } else
      return false
  }

  /** Finds all possible candidates
  * Returned list is sorted by numbered walls as they are more difficult to solve */
   def find_tiles(board:Matrix, condition: Char => Boolean): List[Position] = {
   (for
     {
       row <- board.indices
       col <- board.head.indices
       if(condition(board(row)(col)))
     } yield Position(row,col)).toList
   }


  def backtracking(board: Matrix, candidates: List[Position]): Option[Matrix] = {
    visited += 1

    val tempCandidates = remove_walled_candidates(board, candidates) // TODO: Can remove additional candidates during runtime, how efficiten this is requires further testing
   // Check if solvable
    if (tempCandidates.isEmpty) {
      if (check_if_solved(board)) {
        return Option(board)
      }
     return None
    }

    // Check if this node is promising
    promising += 1
    place_light(board, tempCandidates.head).map(newBoard =>
      backtracking(newBoard, filter_litup(board, tempCandidates, tempCandidates.head)).map(solvedBoard =>
        return Option(solvedBoard))) // Success, place light, and go next

    backtracking(board, candidates.filterNot(p => p == tempCandidates.head)).map(solvedBoard =>
      return Option(solvedBoard))

    return None // No solution was found
  }

  /** Filter out empty spaces from candidates that are lit up */
  def filter_litup(board: Matrix, candidates: List[Position], light:Position): List[Position] = {
    return candidates.filterNot(tile =>
      (tile.row == light.row || tile.col == light.col) && // Check if tile and light is on the same row or column
      !(check_wall_between_tiles(board, tile, light))     // Check if there is a wall between tile and light
    )
  }

  /** Finds walls between two given positions on a board
    * Returns false if a wall is found, if no wall is find returns true */
  def check_wall_between_tiles(board: Matrix, tile: Position, light: Position): Boolean = {
    val walls = find_tiles(board, check_tile_if_wall)     // Get all the walls in board

    walls.find(wall =>
      (wall.row == light.row && wall.row == tile.row  &&  // Get walls on the same row as the tile given
      ((wall.col > light.col && wall.col < tile.col)  ||  // Check to the right of light and left of tile
      (wall.col < light.col && wall.col > tile.col))) ||  // Check to the left of light and right of tile
      (wall.col == light.col && wall.col == tile.col  &&  // Get walls on the same row as the tile given
      ((wall.row > light.row && wall.row < tile.row)  ||  // Check if wall position is to the right of light and left of tile
      (wall.row < light.row && wall.row > tile.row)))
    ) match {
      case Some(_) => return true  // A wall was found
      case None    => return false // No wall found
    }
  }

  def sort_number_tiles(board: Matrix, pos1: Position, pos2: Position): Boolean = {
    board(pos1.row)(pos1.col).asDigit > board(pos2.row)(pos2.col).asDigit
  }

  @tailrec
  def place_light_deterministic(board: Matrix): Matrix = {
    // Gets all number tiles that are not satisfied
    val number_tiles = find_tiles(board, check_tile_if_num)
      .filterNot(wall =>
        get_number_of_lights_around_number(board, wall) == board(wall.row)(wall.col).asDigit)
      .sortWith(sort_number_tiles(board, _:Position, _:Position))

    var newBoard:Matrix = board

    // Checks if we can place a light adjacent to num tile
    number_tiles.foreach(num_pos => {
      val adjacent_empty = check_adjacent(newBoard, num_pos, check_placement)
      val nr_of_adjacent_lights = get_number_of_lights_around_number(newBoard, num_pos)

      // Checks if adjacent empty plus the amount of lights is greater then the number
      if (adjacent_empty.length + nr_of_adjacent_lights == newBoard(num_pos.row)(num_pos.col).asDigit)
        adjacent_empty.foreach( tile =>
          place_light(newBoard, tile).foreach(b => newBoard = b)// Replaces board
        )
    })
    // If there was any lights placed there may be new lights that can be placed
    if (newBoard == board)
      return newBoard
    else
      return place_light_deterministic(newBoard)
  }


  /** Checks if every wall in a given board have the required amount of lights around it
    * If the required amount of lights is fulfilled, remove the remaining empty tiles from the candidates list */
  def remove_walled_candidates(board: Matrix, candidates: List[Position]): List[Position] = {
    val walls = find_tiles(board, check_tile_if_num)       // Find every wall on the board
    .filter(wall =>                                         // Get all the walls who are completed
      get_number_of_lights_around_number(board, wall) ==    // Check if wall have the required lights around itself
        board(wall.row)(wall.col).asDigit)
    .flatMap(wall => get_adjacent(board, wall))             // Get all the empty tiles adjacent to completed walls
    return candidates.filterNot(pos => walls.contains(pos)) // Remove those empty tiles from the candidates list
  }

  /** Removes all the tiles that have been lit up */
  def remove_litup_candidates(board: Matrix, candidates: List[Position], lights: List[Position]): List[Position] = {
    if(lights.nonEmpty && candidates.nonEmpty)  // Check if there are any lights left, and there are still candidates to remove
      return remove_litup_candidates(board, filter_litup(board, candidates, lights.head), lights.filterNot(p => p == lights.head))
    return candidates
  }

  /** Gets all the candidates in a board */
  def get_candidates(board: Matrix): List[Position] = {
    remove_litup_candidates(
      board,
      remove_walled_candidates(board, find_tiles(board, check_tile_if_Empty)),
      find_tiles(board, solver.check_tile_if_light)
    )
  }

  def sort_candidates(board: Matrix, candidates: List[Position]): List[Position] = {
    val number_tiles: List[Position] = find_tiles(board, check_tile_if_num)   // Get all the numbered tiles
      .sortWith(sort_number_tiles(board, _:Position, _:Position))             // Sort by highest number
      .flatMap(pos => get_adjacent(board, pos))                               // Get the adjacent tiles of the numbered tiles
      .intersect(candidates)                                                  // Keep tiles that is common with candidates
    println("Sorted: " + number_tiles)
    return number_tiles ::: candidates.diff(number_tiles)                     // Append the remaining candidates to the sorted empty tiles
  }
}
