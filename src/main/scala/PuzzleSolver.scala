
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
    puzzle.printBoard
    timer.start()
    val board = solver.place_light_deterministic(puzzle.board)
    val temp: Option[Matrix] = solver.backtracking(board, solver.find_tiles(board, solver.check_tile_if_Empty))
    timer.stop()

    // Prints
    solver.print_board(temp.get)
    println("Promising: " + solver.promising + "\n" + "Visited: " + solver.visited)
    println("Time: " + timer.stop.getElapsedTime())

    return new Puzzle(puzzle.sizeX, puzzle.sizeY, temp, puzzle.board)
  }

    putSolution(args(1), solve(getPuzzle(args(0))))
 


}


object testPuzzleSolver extends App {
  val id_7x7_c3mBc2a2a2a0c2m0c: Matrix = List(
    "_ _ * 3 * _ _".toList.filter(filter_space),
    "_ _ _ * _ _ _".toList.filter(filter_space),
    "_ _ _ 2 _ _ _".toList.filter(filter_space),
    "2 _ 2 * 2 _ 0".toList.filter(filter_space),
    "_ _ _ 2 _ _ _".toList.filter(filter_space),
    "_ _ _ _ _ _ _".toList.filter(filter_space),
    "_ _ _ 0 _ _ _".toList.filter(filter_space)
  )

  println(solver.place_light(id_7x7_c3mBc2a2a2a0c2m0c, Position(4, 2)))

  val timer = new Stopwatch() // Use to take realtime
  // Herusitic tricks
  val board = solver.place_light_deterministic(Boards.id_7x7_c3mBc2a2a2a0c2m0c)
  solver.print_board(board)
//
//  timer.start()
//  val temp = solver.backtracking(board, solver.find_tiles(board, solver.check_tile_if_Empty))
//  timer.stop()

  // Prints
//  println(temp)
//  println("Promising: " + solver.promising + "\n" + "Visited: " + solver.visited)
//  println("Time: " + timer.stop.getElapsedTime())

  // Post processing


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
  // TODO: Find numbered walls first, append empty positions first, start with walls=4..1
   def find_tiles(board:Matrix, condition: Char => Boolean): List[Position] = {
   (for
     {
       row <- board.indices
       col <- board.head.indices
       if(condition(board(row)(col)))
     } yield new Position(row,col)).toList
   }
 
  def backtracking(board: Matrix, candidates: List[Position]): Option[Matrix] = {
    // print_board(board)
    // println("Candidates: " + candidates)

      // Check if finished

    // This node is visited
    visited += 1

   // Check if solvable
    if (candidates.isEmpty) {
      if (check_if_solved(board)) {
        // println("Candidates: " + candidates)
        // print_board(board)
        return Option(board)
      }
     return None
   }

      // Check if this node is promising
      promising += 1
      place_light(board, candidates.head).map(newBoard =>
        backtracking(newBoard, filter_litup(board, candidates, candidates.head)).map(x => return Option(x))) // Success, place light, and go next

      backtracking(board, candidates.filterNot(p => p == candidates.head)).map(x => return Option(x))
    return None // No solution was found
  }

  /** Filter out empty spaces from candidates that are lit up */
  def filter_litup(board: Matrix, candidates: List[Position], light:Position): List[Position] = {
    return candidates.filterNot(tile =>  
      tile.row == light.row &&                             // Check if tile and light is on the same row
      !(check_wall_between_tiles(board, tile, light)) ||   // Check if there is a wall between tile and light
      tile.col == light.col &&                             // Check if the tile and light is on the same column
      !(check_wall_between_tiles_two(board, tile, light))  // Check if there is a wall between tile and light
    )
  }

  /** Finds walls between two given positions on a board
    * Returns false if a wall is found, if no wall is find returns true */
  // TODO: Combine these to one function, tried to transpose board, is promising but require further tweaks
  def check_wall_between_tiles(board: Matrix, tile: Position, light: Position): Boolean = {
    val walls = find_tiles(board, check_tile_if_wall)    // Get all the walls in board

    walls.find(wall => wall.row == light.row         &&  // Get walls on the same row as the tile given
      ((wall.col > light.col && wall.col < tile.col) ||  // Check to the right of light and left of tile
      (wall.col < light.col && wall.col > tile.col))     // Check to the left of light and right of tile
    ) match {
      case Some(_) => return true  // A wall as found
      case None    => return false // No wall found
    }
  }

  def check_wall_between_tiles_two(board: Matrix, tile: Position, light: Position): Boolean = {
    val walls = find_tiles(board, check_tile_if_wall)    // Get all the walls in board

    walls.find(wall =>  wall.col == light.col        &&  // Get walls on the same row as the tile given
      ((wall.row > light.row && wall.row < tile.row) ||  // Check if wall position is to the right of light and left of tile
      (wall.row < light.row && wall.row > tile.row))     // Check if wall position is to the left of light and right of tile
    ) match {
      case Some(_) => return true
      case None    => return false
    }
  }

  def sort_number_tiles(board: Matrix, pos1: Position, pos2: Position): Boolean = {
    board(pos1.row)(pos1.col).asDigit > board(pos2.row)(pos2.col).asDigit
  }

  // TODO: pass list of numbers tiles that hasn't any lights around them, instead of checking all numbers on the board
  @tailrec
  def place_light_deterministic(board: Matrix): Matrix = {
    val number_tiles = find_tiles(board, check_tile_if_num)
      .sortWith(sort_number_tiles(board, _:Position, _:Position))
    // TODO: filter out numbers with max num of lights

    var newBoard:Matrix = board

    // Checks if we can place a light adjacent to num tile
    number_tiles.foreach( num => {
      val adjacent_empty = check_adjacent(newBoard, num, check_placement)
//      println("-----\n" + "Posisiton: " + num + ", char: " + newBoard(num.row)(num.col) + "\nadjacent_empty: " + adjacent_empty)
//      println("adjacent_empty: " + adjacent_empty)

      if (adjacent_empty.length <= newBoard(num.row)(num.col).asDigit) {
        adjacent_empty.foreach( tile => {
          place_light(newBoard, tile) match {
            case Some(b) => newBoard = b    // Replaces board
            case None    =>                 // This happens often, see todo
          }
        })
      }
    })
    // If there was any lights placed there may be new lights that can be placed
    if (newBoard == board) {
      return newBoard
    } else
      place_light_deterministic(newBoard)
  }
}
