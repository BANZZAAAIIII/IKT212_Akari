// Puzzle class represents the puzzle
import com.akari.types._

class Puzzle(x: Int, y: Int, sol: String, _board: Matrix) {

  val sizeX: Int = x;
  val sizeY: Int = y;
  val solution: String = sol;
  val board: Matrix = _board

  override def toString: String = {
    s"${sizeX}x${sizeY} -->\n${solution} \n" +
      s"${printBoard}"
  }

  // Converts board to rows of strings
  def printBoard = {
    board.foreach(s => println(s.mkString))
  }
}