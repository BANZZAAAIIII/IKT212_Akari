import java.io.FileWriter
import scala.io._
import com.akari.types._

object PuzzleReaderWriter{

    /** Reads a puzzle from file */
  def getPuzzle(infile: String): Puzzle = {
    val lines = Source.fromFile(infile).getLines().toList
    val sizeNumbers: Array[String] = lines.filter(_ startsWith ("size")).head.split(" ").last.split("x") // Get the sizes
    val board: Matrix = lines.slice(2, sizeNumbers.last.toInt + 2).map(_.toList)                                   // Get the puzzle

    return new Puzzle(
      row = sizeNumbers(0).toInt,
      col = sizeNumbers.last.toInt,
      sol = None,
      _board = board
      )
  }
  /** Writes a puzzle to a given file */
  def putSolution(outfile: String, puzzle: Puzzle): Unit = {
    val fw = new FileWriter(outfile, false)                    // Open file
    fw.write("puzzles 1\n")                                       // Cheating to simplify code, as we do not need to read or write more than one puzzle per file
    fw.write("size " + puzzle.sizeRow + "x" + puzzle.sizeCol + "\n")  // Write the size
    fw.write(puzzle.solution.fold("No solution found")(board =>        // Check if solution was found
      board.map(row => row.mkString + "\n").mkString))                 // Transform board to strings, and write to file
    fw.close                                                           // Close the file
  }
}