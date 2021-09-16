/**
  *   TODO: Make a dictionary "x" = x, "y" = y
  */

import PuzzleReaderWriter.{closing, getNumPuzzles, getPuzzle, initRW, putSolution}
import com.akari.types._
import stopwatch.Stopwatch

import scala.::
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.HashMap

object PuzzleSolver extends App{

  def filter_space(c: Char): Boolean = c != ' '
  val simple_board: Matrix = List(
    "_ 1 _".toList.filter(filter_space),
    "_ X _".toList.filter(filter_space),
    "_ 2 _".toList.filter(filter_space),
    "_ _ 2".toList.filter(filter_space),
    "_ _ _".toList.filter(filter_space),
    "_ _ _".toList.filter(filter_space)
  )
  // Solver function
  def solve(puzzle:Puzzle): Puzzle = {
    val timer = new Stopwatch() // Use to take realtime
    // Herusitic tricks
    timer.start()
    val temp = solver.backtracking(simple_board, solver.find_candidates(simple_board))
    timer.stop()

    // Prints
    println(temp)
    println("Promising: " + solver.promising + "\n" + "Visited: " + solver.visited)
    println("Time: " + timer.stop.getElapsedTime())

    // println(solver.find_candidates(simple_board))
    // Backtracking
    //backtracking(puzzle.board)
    
    // Post processing

    // we predefine just two solutions
    val solution7x7 =
        "* _ _ _ _ _ _\n"+
        "2 _ _ * _ _ _\n"+
        "* 1 _ 2 0 _ *\n"+
        "X _ _ * _ _ _\n"+
        "X _ * _ _ _ _\n"+
        "* _ _ _ _ 0 _\n"+
        "2 * X _ * _ _"
    val solution10x5 =
        "_ _ _ _ * _ _ _ 1 *\n"+
        "_ * 1 0 _ * X _ 0 _\n"+
        "_ _ _ _ _ _ _ * _ 1\n"+
        "_ _ * 2 _ 1 * _ X *\n"+
        "_ _ _ * _ _ _ 1 * _"
    val size = puzzle.sizeX*100 + puzzle.sizeY
    val solution = size match {
      case 707  => solution7x7
      case 1005 => solution10x5
      case _    => "cannot solve this puzzle"
    }


    println(puzzle.toString())
    return new Puzzle(puzzle.sizeX, puzzle.sizeY, puzzle.id, puzzle.difficulty, puzzle.symmetry, puzzle.black_percent, puzzle.solution, puzzle.board)
  }

  initRW(args(0),args(1)) // read argument, file 1 is file containing puzzles, file 2 is file to write solutions

  val numPuzzles = getNumPuzzles // Holds number of puzzles in file 1

  // Looper til antall puzzles, henter puzzle fra en liste, løser puzzle, skriver puzzle til fil
  for (count<- 0 until numPuzzles) {
    println("Solving puzzle #"+(count+1).toString)
    putSolution(solve(getPuzzle(count)))
  }

  println("Processed " + numPuzzles.toString + " puzzles.")
  closing()
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
    case One | Two | Three | Four  => true
    case _ => false
  }

  /** Returns true when given a empty tile */
  def check_tile_for_Empty(c: Char): Boolean = if (c == Empty) true else false

  /** Returns true when given a light */
  def check_tile_for_light(c: Char): Boolean = if (c == Light) true else false

  def filter_space(c: Char): Boolean = c != ' '
  val simple_board: Matrix = List(
    "_ 1 _".toList.filter(filter_space),
    "_ X _".toList.filter(filter_space),
    "_ 2 _".toList.filter(filter_space),
    "_ _ 2".toList.filter(filter_space),
    "_ _ _".toList.filter(filter_space),
    "_ _ _".toList.filter(filter_space)
  )

  val simple_solved_board: Matrix = List(
    "_ 1 *".toList.filter(filter_space),
    "_ X _".toList.filter(filter_space),
    "* 2 _".toList.filter(filter_space),
    "_ * 2".toList.filter(filter_space),
    "_ _ *".toList.filter(filter_space),
    "_ _ _".toList.filter(filter_space)
  )


  /** Checks a list if there is a light in it
   *  Takes a Range to iterate over the list */
  def check_list(board:List[Char], range: Range): Boolean = {
    // TODO: exception handling
    for(i <- range) {
      // TODO: TakeWhile to make an array to a blocking square
      // Use contains to check if there is a light
      val tile = board(i)
      if (!check_tile_for_Empty(tile)) {
        return !check_tile_for_light(tile)
      }
    }
    return true
  }

  /** Checks all sides of a x, y pos for a light.
   *  Returns true of there is no light on the same row or col blocking
   */
  def check_placement(board: Matrix, pos: Position): Boolean = {
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
   *  Returns None if placement is illegal or a new board if not
   */
  def place_light(board: Matrix, pos: Position): Option[Matrix] = {
    // println("X: " + pos.col + " Y: " + pos.row)
    if (!check_tile_for_Empty(board(pos.row)(pos.col))) return None

    val adjacent_numbers = check_adjacent(board, pos, check_tile_if_num)
    for (num <- adjacent_numbers) {
      val nr_of_light = get_number_of_lights_around_number(board, num)
      if (nr_of_light >= board(num.row)(num.col).asDigit)
        return None
    }

    if (check_placement(board, pos)) {
      val newBoard = board.updated(pos.row, board(pos.row).updated(pos.col, Light))
      return Option(newBoard)
    } else
      return None
  }

  /** Checks all valid positions adjacent to x, y pos on board.
   *  Takes a functions, condition: Char => Boolean, to check adjacent squares.
   *  Returns all x, y positions where condition is true */
  def check_adjacent(board: Matrix, pos: Position, condition: Char => Boolean): List[Position] = {
    val positions = new ListBuffer[Position]()

    // TODO: improve this by making it more functional
    // Check Left cell
    if (!(pos.col - 1 < 0) && condition(board(pos.row)(pos.col - 1)))
      positions += new Position(pos.row, pos.col - 1)

    // Check Right Cell
    if (!(pos.col + 1 > board.head.length - 1) && condition(board(pos.row)(pos.col + 1)))
      positions += new Position(pos.row, pos.col + 1)

    // Check Top Cell
    if (!(pos.row - 1 < 0) && condition(board(pos.row - 1)(pos.col)))
      positions += new Position(pos.row - 1, pos.col)

    // Check Bottom Cell
    if (!(pos.row + 1 > board.length - 1) && condition(board(pos.row + 1)(pos.col)))
      positions += new Position(pos.row + 1, pos.col)

    return positions.toList
  }

  /** Gets number of lights adjacent to a square */
  def get_number_of_lights_around_number(board: Matrix, pos: Position): Int =
    check_adjacent(board, pos, check_tile_for_light).length

  /** Checks the puzzle is solved
   *  Doesn't check if lights are placed incorrectly */
  def check_if_solved(board: Matrix): Boolean = {
    for (row <- board.indices;
         col <- board.head.indices)
    {
      if (board(row)(col) != Wall) {
        if (check_tile_if_num(board(row)(col))) {
          // Checks if correct num if lights are adjacent to number wall
          val nr_of_light = get_number_of_lights_around_number(board, new Position(row,col))

          if (!(nr_of_light == board(row)(col).asDigit))
            return false

        } else {
          // Check if that there is a light on current square and returns false it not.
          if (check_placement(board, new Position(row,col))) return false
        }
      }
    }

    return true
  }

  /** Finds all possible candidates
  * Returned list is sorted by numbered walls as they are more difficult to solve
   */
  // TODO: Find numbered walls first, append empty positions first, start with walls=4..1
   def find_candidates(board:Matrix): List[Position] = {
   (for
     {
       row <- board.indices;
       col <- board.head.indices
       if(check_tile_for_Empty(board(row)(col)))
     } yield new Position(row,col)).toList
   }
 
  def backtracking(board: Matrix, candidates: List[Position]): Boolean = {
    // printer(board)
    // println("Candidates: " + candidates)

    // Check if finished
    if (check_if_solved(board)) {
      print_board(board)
      return true
    }

    // This node is visited
    visited += 1

   // Check if solvable
   if (candidates.isEmpty) {
     return false
   }

    for (pos <- candidates) {
      // Check if this node is promising
      promising += 1

      // Remove candidates
      val temp = candidates.filterNot(p => p == pos)
      place_light(board, pos) match {
        case Some(newBoard) => if (backtracking(newBoard, temp)) return true
        case None           => if (backtracking(board, temp)) return true
      }
    }

    return false
  }
}
