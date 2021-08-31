import PuzzleReaderWriter.{closing, getNumPuzzles, getPuzzle, initRW, putSolution}
import Const._

object PuzzleSolver extends App{

  // Solver function
  def solve(puzzle:Puzzle): Puzzle = {
    
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
  def print_board(a: List[List[Char]]): Unit = {
    a.foreach(y => {
      y.foreach(x => print(x + " "))
      println("")
    })
  }

  /** Returns true when given a empty tile */
  def check_tile_for_Empty(c: Char): Boolean = {
    c match {
      case Empty => true
      case _    => false
    }
  }

  /** Returns true when given a light */
  def check_tile_for_light(c: Char): Boolean = {
    c match {
      case Light => true
      case _     => false
    }
  }


  def filter_space(c: Char): Boolean = c != ' '
  val simple_board_X_44: List[List[Char]] = List(
    "_ _ X *".toList.filter(filter_space),
    "X _ _ X".toList.filter(filter_space),
    "_ _ * _".toList.filter(filter_space),
    "_ _ _ _".toList.filter(filter_space)
  )

  /** Checks all sides of a x, y pos for a light
   *  Returns true of there is no light on the same row or col blocking
   */
  def check_placement(board: List[List[Char]], x: Int, y: Int): Boolean = {
    def check_list(board:List[Char], range: Range): Boolean = {
      for(i <- range) {
        val tile = board(i)
        if (!check_tile_for_Empty(tile)) {
          return !check_tile_for_light(tile)
        }
      }
      return true
    }

    if (!check_tile_for_Empty(board(y)(x)))
      return false

    // Checks for a light in a positive direction for x, y pos
    val pos =
      (check_list(board(y), x until board.head.length)
      &&
      check_list(for(a <- board) yield a(x), y until board.length))

    // Checks for a light in a negative direction for x, y pos
    val neg =
      (check_list(board(y), (0 until x).reverse)
      &&
      check_list(for(a <- board) yield a(x), (0 until y).reverse))

    return pos && neg
  }

   /** places a light on a x, y position if the tile is valid
    *  Returns None if placement is illegal or a new board if not
    */
  def place_light(board: List[List[Char]], x:Int, y:Int): Option[List[List[Char]]] = {
    if (check_placement(board, x, y)) {
      val newBoard = board.updated(y, board(y).updated(x, Light))
      return Option(newBoard)
    } else
      return None
  }

  // usage example
  val x = 0
  val y = 0
  place_light(simple_board_X_44, x, y) match {
    case Some(b) => print_board(b)
    case None    => println("Illegal move")
  }
}