/**
  *   TODO: Make a dictionary "x" = x, "y" = y
  */


import PuzzleReaderWriter.{closing, getNumPuzzles, getPuzzle, initRW, putSolution}
import com.akari.types._

import scala.::
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.HashMap

object PuzzleSolver extends App{

  // Solver function
  def solve(puzzle:Puzzle): Puzzle = {

    // Herusitic tricks
    println(solver.place_next(puzzle.board))
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

  // Looper til antall puzzles, henter puzzle fra en liste, løser puzzle, skrivr puzzle til fil
  for (count<- 0 until numPuzzles) {
    println("Solving puzzle #"+(count+1).toString)
    putSolution(solve(getPuzzle(count)))
  }

  println("Processed " + numPuzzles.toString + " puzzles.")
  closing()
}

object solver extends App {
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
    "_ 1 X *".toList.filter(filter_space),
    "2 _ * X".toList.filter(filter_space),
    "_ _ _ _".toList.filter(filter_space),
    "_ _ _ _".toList.filter(filter_space)
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
    lazy val positiveXDir: Boolean = check_list(board(pos.y), pos.x until board.head.length)
    lazy val positiveYDir: Boolean = check_list(for(a <- board) yield a(pos.x), pos.y until board.length)
    if (!(positiveXDir && positiveYDir)) return false

    // Checks for a light in a negative direction for x, y pos
    lazy val negativeXDir: Boolean = check_list(board(pos.y), (0 until pos.x).reverse)
    lazy val negativeYDir: Boolean = check_list(for(a <- board) yield a(pos.x), (0 until pos.y).reverse)
    return negativeXDir && negativeYDir
  }

  /** Places a light on a x, y position if the tile is valid.
   *  Returns None if placement is illegal or a new board if not
   */
  // TODO: Use position class
  def place_light(board: Matrix, pos: Position): Option[Matrix] = {
    if (!check_tile_for_Empty(board(pos.y)(pos.x))) return None

    val adjacent_numbers = check_adjacent(board, pos, check_tile_if_num)
    for (num <- adjacent_numbers) {
      val nr_of_light = get_number_of_lights_around_number(board, num)
      if (nr_of_light >= board(num.y)(num.x).asDigit)
        return None
    }


    if (check_placement(board, pos)) {
      val newBoard = board.updated(pos.y, board(pos.y).updated(pos.x, Light))
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
    if (!(pos.x - 1 < 0) && condition(board(pos.y)(pos.x - 1)))
      positions += new Position(pos.x - 1, pos.y)

    if (!(pos.x + 1 > board.head.length - 1) && condition(board(pos.y)(pos.x + 1)))
      positions += new Position(pos.x + 1, pos.y)

    if (!(pos.y - 1 < 0) && condition(board(pos.y - 1)(pos.x)))
      positions += new Position(pos.x, pos.y - 1)

    if (!(pos.y + 1 > board.length - 1) && condition(board(pos.y + 1)(pos.x)))
      positions += new Position(pos.x, pos.y + 1)

    return positions.toList
  }

  /** Gets number of lights adjacent to a square */
  def get_number_of_lights_around_number(board: Matrix, pos: Position): Int =
    check_adjacent(board, pos, check_tile_for_light).length

  /** Checks the puzzle is solved
   *  Doesn't check if lights are placed incorrectly */
  def check_if_solved(board: Matrix): Boolean = {
    for (y <- board.indices;
         x <- board.head.indices)
    {
      if (board(y)(x) != Wall) {
        if (check_tile_if_num(board(y)(x))) {
          // Checks if correct num if lights are adjacent to number wall
          val nr_of_light = get_number_of_lights_around_number(board, new Position(x,y))

          if (!(nr_of_light == board(y)(x).asDigit))
            return false

        } else {
          // Check if that there is a light on current square and returns false it not.
          if (check_placement(board, new Position(x,y))) return false
        }
      }
    }

    return true
  }

  
  def place_next(board:Matrix): Option[Position] = {
  (for
    {
      y <- board.indices;
      x <- board.head.indices
      if(check_tile_for_Empty(board(y)(x)))
    } yield new Position(y,x)).headOption
  }
 
  def backtracking(board: Matrix, pos: Position): Boolean = {

    // Done?
    if (pos.x == board.head.length && pos.y == board.length) { // If last position on board
      return check_if_solved(board)
    }
    // Promising
    //place_light(board, pos.x, pos.y)
    // Next 
    return false
  } 
  
}
