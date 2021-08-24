// Puzzle class represents the puzzle
class Puzzle(x: Int, y: Int, sol :String) {
  val sizeX: Int = x;
  val sizeY: Int = y;
  val solution: String = sol;

  override def toString: String = {
    s"${sizeX}x${sizeY} -->\n${solution}"
  }
}