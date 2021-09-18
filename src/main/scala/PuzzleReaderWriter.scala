import java.io.FileWriter
import scala.io._
import com.akari.types._

object PuzzleReaderWriter{

    /**
    * Reads a puzzle from file
    */
  def getPuzzle(infile: String): Puzzle = {
    val lines = Source.fromFile(infile).getLines().toList
    val sizeNumbers: Array[String] = lines.filter(_ startsWith("size"))(0).split(" ").last.split("x") // Get the sizes
    val board: Matrix = lines.slice(2, sizeNumbers.last.toInt + 2).map(_.toList)                            // Get the puzzle
    return new Puzzle(
      sizeNumbers(0).toInt,   // Columns
      sizeNumbers.last.toInt, // Rows
      None,                     // Solution
      board                   // Board
      )
  }
  /**
    *  Writes a puzzle to a given file
    */
  def putSolution(outfile:String, puzzle: Puzzle): Unit = { 
    val fw = new FileWriter(outfile, false)                       // Open file
    fw.write("puzzles 1\n")                                       // Cheating to simplify code, as we do not need to read or write more than one puzzle per file
    fw.write("size " + puzzle.sizeX + "x" + puzzle.sizeY + "\n")  // Write the size
    puzzle.solution.get.foreach(s => fw.write(s.mkString + "\n")) // Write the puzzle
    fw.close                                                      // Close the file
  }
}