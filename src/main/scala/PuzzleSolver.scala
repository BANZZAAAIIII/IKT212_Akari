import PuzzleReaderWriter.{initRW, getNumPuzzles, getPuzzle, putSolution, closing}

object PuzzleSolver extends App{

  // Solver function
  def solve(puzzle:Puzzle): Puzzle = {
    println(puzzle)

    val id: String = "%id 13x9:1cBBBdBBbBa1b1B1b0cBBcB0d1BbBa1eBaB01bBd0fB1aBBBB001d0a0bBBb11aBBc3a0g1d"
    val difficulty: Int = 1
    val symmetry: Int = 1
    val black_percent = 40
    
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
    return new Puzzle(puzzle.sizeX, puzzle.sizeY, id, difficulty, symmetry, black_percent, solution)
  }

  initRW(args(0),args(1)) // read argument, file 1 is file containing puzzles, file 2 is file to write solutions

<<<<<<< HEAD
  val numPuzzles=getNumPuzzles
=======
  val numPuzzles = getNumPuzzles // Holds number of puzzles in file 1
>>>>>>> 6fcc700 (Added new attributes to puzzles, and read them from file, added example puzzles)

  // Looper til antall puzzles, henter puzzle fra en liste, løser puzzle, skrivr puzzle til fil
  for (count<- 0 until numPuzzles) {
    println("Solving puzzle #"+(count+1).toString)
    putSolution(solve(getPuzzle(count)))
  }

  println("Processed " + numPuzzles.toString + " puzzles.")
  closing()
}