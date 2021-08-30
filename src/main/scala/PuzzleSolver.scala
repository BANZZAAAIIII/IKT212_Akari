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
  def print_board(a: Array[Array[Char]]): Unit = {
    a.foreach(y => {
      y.foreach(x => print(x))
      println("")
    })
  }

  /** Returns true when given a empty tile */
  def check_tile_for_Empty(c: Char): Boolean = {
    c match {
      case Empty => true
      case One => true
      case Two => true
      case Three => true
      case Four => true
      case _    => false
    }
  }

  /** Checks if its possible to place light a light
    * Returns false is tile is anything other then _ (empty) */
  def check_tile_for_valid_placement(c: Char): Boolean = c match {
    case Empty => true
    case _     => false
  }

  /** Returns true when given a light */
  def check_tile_for_light(c: Char): Boolean = {
    c match {
      case Light => true
      case _     => false
    }
  }

  def filter_space(c: Char): Boolean = c != ' '

  //val simple_board_X_44 = Array.ofDim[Char](5,4)
  //simple_board_X_44(0) = "1 1 1 1".toArray.filter(filter_space)
  //simple_board_X_44(1) = "2 2 2 2".toArray.filter(filter_space)
  //simple_board_X_44(2) = "3 3 3 3".toArray.filter(filter_space)
  //simple_board_X_44(3) = "4 4 4 4".toArray.filter(filter_space)
  //simple_board_X_44(4) = "5 5 5 5".toArray.filter(filter_space)

  val simple_board_X_44: Array[Array[Char]] = Array.ofDim[Char](4,4)
  simple_board_X_44(0) = "_ _ X *".toArray.filter(filter_space)
  simple_board_X_44(1) = "X _ _ X".toArray.filter(filter_space)
  simple_board_X_44(2) = "_ _ * _".toArray.filter(filter_space)
  simple_board_X_44(3) = "_ _ _ _".toArray.filter(filter_space)

  def check_array(board:Array[Char], range: Range): Boolean = {
    //print("\nchecking array: ")
    //board.foreach(print(_))
    //println("")
    for(i <- range) {
      val tile = board(i)
      //print(tile)
      if (!check_tile_for_Empty(tile)) {
        return !check_tile_for_light(tile)
      }
    }
    return true
  }


  /** Checks all sides of a x, y pos for a light
   *  Returns true of there is no light on the same row or col blocking
   */
  def check_placement(board: Array[Array[Char]], x: Int, y: Int): Boolean = {
    if (!check_tile_for_valid_placement(board(y)(x)))
      return false

    val pos =
      (check_array(board(y), x until simple_board_X_44(0).length)
      &&
      check_array(for(a <- simple_board_X_44) yield a(x), y until simple_board_X_44.length))

    val neg =
      (check_array(board(y), (0 until x).reverse)
      &&
      check_array(for(a <- simple_board_X_44) yield a(x), (0 until y).reverse))

    return pos && neg
  }

   /** places a light on a x, y position if the tile is valid
    *  Returns None if placement is illegal or a new board if not
    */
  def place_light(board: Array[Array[Char]], x:Int, y:Int): Option[Array[Array[Char]]] = {
    if (check_placement(board, x, y)) {
      board(y)(x) = '*'
      return (Option(board))
    } else
      return None
  }

  val x = 1
  val y = 2

  val newboard = place_light(simple_board_X_44, x, y)
  print_board(newboard.getOrElse(simple_board_X_44))

}