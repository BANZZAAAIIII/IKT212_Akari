// Puzzle class represents the puzzle
class Puzzle(x: Int, y: Int, _id: String, _difficulty: Int, _symmetry: Int, _black_percent: Int, sol :String) {
  val id: String = _id;
  val difficulty: Int = _difficulty;
  val symmetry: Int = _symmetry;
  val black_percent: Int = _black_percent;
  val sizeX: Int = x;
  val sizeY: Int = y;
  val solution: String = sol;


  override def toString: String = {
    s"${sizeX}x${sizeY} -->\n${solution}"
  }
}