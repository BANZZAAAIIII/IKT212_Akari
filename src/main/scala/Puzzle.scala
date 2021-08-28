// Puzzle class represents the puzzle
class Puzzle(x: Int, y: Int, _id: String, _difficulty: Int, _symmetry: Int, _black_percent: Int, sol :String, _board: Array[Array[Char]]) {
  val id: String = _id;
  val difficulty: Int = _difficulty;
  val symmetry: Int = _symmetry;
  val black_percent: Int = _black_percent;
  val sizeX: Int = x;
  val sizeY: Int = y;
  val solution: String = sol;

  val board: Array[Array[Char]] = _board

  override def toString: String = {
    s"${sizeX}x${sizeY} -->\n${solution} \n" +
      s"${printBoard}"
  }

  // TODO: Implement board presentation
  def printBoard = {
    print("IMPLEMENT THIS")
  }
}