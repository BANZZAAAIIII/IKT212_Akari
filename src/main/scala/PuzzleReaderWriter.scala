import java.io.FileWriter
import scala.io._


// TODO: Look at currying/partial functions
object PuzzleReaderWriter{
  var unsolvedFile:String="";
  var solvedFile:String="";
  var lines:List[String]=Nil;
  var fw:FileWriter=null;

  // TODO: Wrap functino in IO tag to better signal that this is an impure function
  def initRW(infile:String, outfile:String): Unit ={ // Initiate read write process
    unsolvedFile = infile
    solvedFile = outfile
    lines = Source.fromFile(unsolvedFile).getLines().toList
    fw = new FileWriter(solvedFile, false)
  }

  // Gets the number of puzzles in file from first line in the file
  def getNumPuzzles: Int = {
    val countPuzzles = lines(0).split(" ").last.toInt
    // writing number of puzzles into solution
    fw.write("puzzles " + countPuzzles.toString+"\n")
    return countPuzzles
  }

  // Read size of puzzle, then read puzzle lines
  // Filter items such that only ex: size is left and the index placement will correspond to index puzzle item
  // TODO: Reduce number of operations to one filter read all meta data?
  // TODO: Read board data
  // 1. Filter on ID
  // 2. Filter out that above
  // 3. Filter out that below
  // 4. Read id, size, diff, symm, black_percent, board
  def getPuzzle(index:Int): Puzzle = {
    val sizeNumbers: Array[String] = lines.filter(_ startsWith("%size"))(index).split(" ").last.split("x")
    val id: String = lines.filter(_ startsWith("%id"))(index).split(" ").last
    val difficulty: Int = lines.filter(_ startsWith("%difficulty"))(index).split(" ").last.toInt
    val symmetry: Int = lines.filter(_ startsWith("%symmetry"))(index).split(" ").last.toInt
    val black_percent = lines.filter(_ startsWith("%black_percent"))(index).split(" ").last.toInt
    
    // Identify puzzly by ID
    val indexId: Int = lines.indexOf(lines.filter(_ startsWith("%id"))(index))    
    val board: Array[Array[Char]] = getRows(indexId, Array.ofDim[Char](sizeNumbers(0).toInt, sizeNumbers.last.toInt))
  
    
    return new Puzzle(
      sizeNumbers(0).toInt,
      sizeNumbers.last.toInt,
      id,
      difficulty,
      symmetry,
      black_percent,
      "",
      board)
  }

  def putSolution(puzzle: Puzzle): Unit = {
    fw.write("size " + puzzle.sizeX + "x" + puzzle.sizeY + "\n")
    fw.write(puzzle.solution + "\n")
  }

  def closing(): Unit = {
    fw.close()
  }

  // TODO: Make more functional with creating new board
  // Recursivly returns a board with the added row
  def getRows(indexId:Int, board: Array[Array[Char]], i:Int = 0): Array[Array[Char]] = {
    if(i < board.size) {
      board(i) = lines((indexId + 4 + i)).toArray
      val y = i + 1 // TODO: Can this be done better in scala?
      getRows(indexId, board, y)
    }
    return board
  }

}