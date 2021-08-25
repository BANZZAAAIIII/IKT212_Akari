import java.io.FileWriter
import scala.io._

object PuzzleReaderWriter{
  var unsolvedFile:String="";
  var solvedFile:String="";
  var lines:List[String]=Nil;
  var fw:FileWriter=null;

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
  def getPuzzle(index:Int): Puzzle = {
    val sizeNumbers = lines.filter(_ startsWith("%size"))(index).split(" ").last.split("x")
    val id: String = lines.filter(_ startsWith("%id"))(index).split(" ").last
    val difficulty: Int = lines.filter(_ startsWith("%difficulty"))(index).split(" ").last.toInt
    val symmetry: Int = lines.filter(_ startsWith("%symmetry"))(index).split(" ").last.toInt
    val black_percent = lines.filter(_ startsWith("%black_percent"))(index).split(" ").last.toInt
    return new Puzzle(
      sizeNumbers(0).toInt,
      sizeNumbers.last.toInt,
      id,
      difficulty,
      symmetry,
      black_percent,
      "")
  }

  def putSolution(puzzle: Puzzle): Unit = {
    fw.write("size " + puzzle.sizeX + "x" + puzzle.sizeY + "\n")
    fw.write(puzzle.solution + "\n")
  }

  def closing(): Unit = {
    fw.close()
  }


}