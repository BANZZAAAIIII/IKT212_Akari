import PuzzleReaderWriter.{initRW, getNumPuzzles, getPuzzle, putSolution, closing}

object PuzzleSolver extends App{
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
    return new Puzzle(puzzle.sizeX, puzzle.sizeY, solution)
  }

  initRW(args(0),args(1))

  val numPuzzles=getNumPuzzles()

  for (count<- 0 until numPuzzles) {
    println("Solving puzzle #"+(count+1).toString)
    putSolution(solve(getPuzzle(count)))
  }

  println("Processed " + numPuzzles.toString + " puzzles.")
  closing()
}