import PuzzleReaderWriter.{closing, getNumPuzzles, getPuzzle, initRW, putSolution}

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
      case '_' => true
      case '1' => true
      case '2' => true
      case '3' => true
      case '4' => true
      case _   => false
    }
  }

  /** Returns true when given a light */
  def check_tile_for_light(c: Char): Boolean = {
    c match {
      case '*' => true
      case _   => false
    }
  }

  def filter_space(c: Char): Boolean = c != ' '

  val simple_board_X_44 = Array.ofDim[Char](5,4)
  simple_board_X_44(0) = "1 _ X _".toArray.filter(_ != ' ')
  simple_board_X_44(1) = "2 X X _".toArray.filter(filter_space)
  simple_board_X_44(2) = "3 _ * _".toArray.filter(filter_space)
  simple_board_X_44(3) = "4 _ _ _".toArray.filter(filter_space)
  simple_board_X_44(4) = "5 _ _ _".toArray.filter(filter_space)

  def check_tile(c: Char): Option[Boolean] = {
    print(c)
    if (!check_tile_for_Empty(c)) {
      return Option(!check_tile_for_light(c))
    }
    return None
  }


  def check_row(board:Array[Char], x: Int): Boolean = {
    // Checks to the right
    def check_row_right():Boolean = {
      println("checking right")
      for(i <- x until board.length) {
        val tile = board(i)
        print(tile)
        if (!check_tile_for_Empty(tile)) {
          return !check_tile_for_light(tile)
        }
      }
      return true
    }
    // Checks to the left
    def check_row_left(): Boolean = {
      println("\nchecking left")
//      for(i <- (0 until x).reverse) if (check_tile(board(i)).getOrElse(false)) return true
      for(i <- (0 until x).reverse) {
        val tile = board(i)
        print(tile)
        if (!check_tile_for_Empty(tile)) {
          return !check_tile_for_light(tile)
        }
      }
      return true
    }

    check_row_right() && check_row_left()
  }

  def check_col(board: Array[Array[Char]], x: Int, y: Int): Boolean = {
    def check_col_up():Boolean = {
      println("checking up")
      for(i <- y until board.length) {
        val tile = board(i)(x)
        print(tile)
        if (!check_tile_for_Empty(tile)) {
          return !check_tile_for_light(tile)
        }
      }
      true
    }

    def check_col_down():Boolean = {
      println("\nchecking down")
      for(i <- (0 until y).reverse) {
        val tile = board(i)(x)
        print(tile)
        if (!check_tile_for_Empty(tile)) {
          return !check_tile_for_light(tile)
        }
      }
      true
    }

    check_col_up() && check_col_down()
  }


  def check_array(board:Array[Char], range: Range): Boolean = {
    print("\nchecking array: ")
    board.foreach(print(_))
    println("")
    for(i <- range) {
      val tile = board(i)
      print(tile)
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
    val pos = check_array(board(x), x until simple_board_X_44(0).length) && check_array(for(a <- simple_board_X_44) yield a(x), x until simple_board_X_44.length)
    val neg = check_array(board(x), (0 until x).reverse) && check_array(for(a <- simple_board_X_44) yield a(x), (0 until x).reverse)

    return pos && neg
  }
  val x = 1
  val y = 1

//  println("\nIs this a valid tile to place a light: " + check_placement(simple_board_X_44, x, y))
//  println("\nIs this a valid tile to place a light: " + check_array(simple_board_X_44(y), (x until simple_board_X_44(0).length)))
//  val col: Array[Char] = for(a <- simple_board_X_44) yield a(0)

    println("\nIs this a valid tile to place a light: " + check_placement(simple_board_X_44, x, y))
    println()
}