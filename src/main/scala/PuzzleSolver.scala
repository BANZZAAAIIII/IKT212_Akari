
import PuzzleReaderWriter.{getPuzzle, putSolution}
import com.akari.types._
import stopwatch.Stopwatch
import utility.Boards
import utility.Util.filter_space


import java.util.concurrent.locks.Condition

import scala.::

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

// Our files
import com.akari.types._
import stopwatch.Stopwatch

object PuzzleSolver extends App{
  // Solver function
  def solve(puzzle:Puzzle): Puzzle = {
    val timer = new Stopwatch() // Use to take realtime

    // Herusitic tricks
    println("Solving puzzle")
    puzzle.printBoard()

    timer.start()
    val board = solver.place_light_deterministic(puzzle.board)

    val candidates = solver.find_tiles(board, solver.check_tile_if_Empty)
    val tempCandidates = solver.remove_walled_candidates(board, candidates)
    val solved_board: Option[Matrix] = solver.backtracking(board, tempCandidates)
    timer.stop()

    // Prints
    solver.print_board(solved_board.get)
    println("Promising: " + solver.promising + "\n" + "Visited: " + solver.visited)
    println("Time: " + timer.stop.getElapsedTime())

    return new Puzzle(puzzle.sizeX, puzzle.sizeY, solved_board, puzzle.board)
  }
    solve(new Puzzle(4, 4, None, Boards.id_7x7_c3mBc2a2a2a0c2m0c))
//  putSolution(args(1), solve(getPuzzle(args(0))))
}

object solver extends App {
  var visited = 0
  var promising = 0

  def print_board(a: Matrix): Unit = {
    a.foreach(y => {
      y.foreach(x => print(x + " "))
      println("")
    })
  }

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

  /** Returns true when given a empty tile */
  def check_tile_if_Empty(c: Char): Boolean = if (c == Empty) true else false

  /** Returns true when given a light */
  def check_tile_if_light(c: Char): Boolean = if (c == Light) true else false

  /** Returns true when given a Zero wall */
  def check_tile_if_zero(c: Char): Boolean = if (c == Zero) true else false

  /** Used to convert a function with signatur, Char => Boolean, to (Matrix, Position) => Boolean */
  def char_to_board_pos(board: Matrix, pos: Position, condition: Char => Boolean): Boolean = {
    condition(board(pos.row)(pos.col))
  }

  /** Checks a list if there is a light in it.
   *  Takes a Range to iterate over the list */
  def check_list(board:List[Char], range: Range): Boolean = {
    // TODO: exception handling
    for(i <- range) {
      // TODO: TakeWhile to make an array to a blocking square
      // Use contains to check if there is a light
      val tile = board(i)
      if (!check_tile_if_Empty(tile)) {
        return !check_tile_if_light(tile)
      }
    }
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
    // println("X: " + pos.col + " Y: " + pos.row)
    if (!check_tile_if_Empty(board(pos.row)(pos.col))) return None

    check_adjacent(board, pos, char_to_board_pos(_:Matrix, _:Position, check_tile_if_num)).foreach( num => {
      val nr_of_light = get_number_of_lights_around_number(board, num)
      if (nr_of_light >= board(num.row)(num.col).asDigit)
        return None
    })

    if (check_placement(board, pos)) {
      return Option(board.updated(pos.row, board(pos.row).updated(pos.col, Light)))
    } else
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
    for (row <- board.indices;
         col <- board.head.indices)
    {
      if (board(row)(col) != Wall) {
        if (check_tile_if_num(board(row)(col))) {
          // Checks if correct num if lights are adjacent to number wall
          val nr_of_light = get_number_of_lights_around_number(board, Position(row, col))

          if (!(nr_of_light == board(row)(col).asDigit))
            return false

        } else {
          // Check if that there is a light on current square and returns false it not.
          if (check_placement(board, Position(row, col))) return false
        }
      }
    }

    return true
  }

  /** Finds all possible candidates
  * Returned list is sorted by numbered walls as they are more difficult to solve */
   def find_tiles(board:Matrix, condition: Char => Boolean): List[Position] = {
   (for
     {
       row <- board.indices
       col <- board.head.indices
       if(condition(board(row)(col)))
     } yield new Position(row,col)).toList
   }


  def backtracking(board: Matrix, candidates: List[Position]): Option[Matrix] = {
    // This node is visited
    visited += 1
    val tempCandidates = remove_walled_candidates(board, candidates) // TODO: Can remove additional candidates during runtime, how efficiten this is requires further testing
   // Check if solvable
    if (tempCandidates.isEmpty) {
      if (check_if_solved(board)) {
        // println("Candidates: " + candidates)
        // print_board(board)
        return Option(board)
      }
     return None
    }

      // Check if this node is promising
      promising += 1
      place_light(board, tempCandidates.head).map(newBoard =>
        backtracking(newBoard, filter_litup(board, tempCandidates, tempCandidates.head)).map(x => return Option(x))) // Success, place light, and go next

      backtracking(board, candidates.filterNot(p => p == tempCandidates.head)).map(x => return Option(x))
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
      .sortWith(sort_number_tiles(board, _:Position, _:Position))
      .filterNot(wall =>                                     
        get_number_of_lights_around_number(board, wall) ==
        board(wall.row)(wall.col).asDigit)

    var newBoard:Matrix = board

    // Checks if we can place a light adjacent to num tile
    number_tiles.foreach(num_pos => {
      val adjacent_empty = check_adjacent(newBoard, num_pos, check_placement)
      val nr_of_adjacent_lights = get_number_of_lights_around_number(newBoard, num_pos)

      // Checks if adjacent empty plus the amount of lights is greater then the number
      if (adjacent_empty.length + nr_of_adjacent_lights <= newBoard(num_pos.row)(num_pos.col).asDigit) {
        adjacent_empty.foreach( tile => {
          place_light(newBoard, tile) match {
            case Some(b) => newBoard = b    // Replaces board
            case None    => println("Optimally, this should not happen :)")
          }
        })
      }
    })
    // If there was any lights placed there may be new lights that can be placed
    if (newBoard == board) {
      return newBoard
    } else
      return place_light_deterministic(newBoard)
  }


  /** Checks if every wall in a given board have the required amount of lights aroun it
   * If the required amount of lights is fullfilled, remove the remaining empty tiles from the candidates list
  */
  def remove_walled_candidates(board: Matrix, candidates: List[Position]): List[Position] = {
    val walls =  find_tiles(board, check_tile_if_num)       // Find every wall on the board
    .filter(wall =>                                         // Get all the walls who are completed
      get_number_of_lights_around_number(board, wall) ==    // Check if wall have the required lights around itself
        board(wall.row)(wall.col).asDigit)
    .flatMap(wall => get_adjacent(board, wall))             // Get all the empty tiles adjacent to completed walls
    return candidates.filterNot(pos => walls.contains(pos)) // Remove those empty tiles from the candidates list
  }

  // TODO: Sort candidates by tiles around higher numbered walls firts
  def sort_candidates(board: Matrix, candidates: List[Position]): List[Position] = {
    val sortedList: ListBuffer[Position] = new ListBuffer[Position]()
    val number_tiles = find_tiles(board, check_tile_if_num)
      .sortWith(sort_number_tiles(board, _:Position, _:Position))
    return candidates
  }
}
