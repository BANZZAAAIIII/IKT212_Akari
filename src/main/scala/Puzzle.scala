// Puzzle class represents the puzzle
import com.akari.types._

class Puzzle(row: Int, col: Int, sol: Option[Matrix], _board: Matrix) {

  val sizeRow: Int = row
  val sizeCol: Int = col
  val solution: Option[Matrix] = sol
  val board: Matrix = _board

  override def toString: String = {
    s"${sizeRow}x${sizeCol} -->\n${solution} \n" +
      s"${printBoard}"
  }

  // Converts board to rows of strings
  def printBoard(): Unit = {
    board.foreach(s => {
      s.foreach(x => print(x + " "))
      println("")
    })
  }

  def printSolvedBoard(): Unit = {
    println("")
    sol match {
      case Some(board) => board.foreach(s => {
        s.foreach(x => print(x + " "))
        println("")
      })
      case None => println("No solution found")
    }
  }
}