package aof
import aof.utils._
object Day12 extends Day with App {

  val day: String = "day12.txt"

  type Vector = (Int, Int)

  type Action = (Char, Int)

  type Position = (Int, Int)

  type Face = Int

  val actions = lines.map(a => (a.charAt(0), a.substring(1).toInt))

  val faces = Vector((0, -1), (1, 0), (0, 1), (-1, 0))

  val facesSize = faces.size

  val turn = Map(('L', -1), ('R', 1))

  def move(a: Action, face: Face): (Face, Vector) = {
    val (dir, r) = a
    dir match {
      case 'N' => face -> (0, -1) * r
      case 'S' => face -> (0, 1) * r
      case 'W' => face -> (-1, 0) * r
      case 'E' => face -> (1, 0) * r
      case 'F' => face -> faces(face) * r
      case 'R' | 'L' =>
        val newFace = (facesSize + (face + turn(dir) * (r / 90))) % facesSize
        newFace -> (0, 0)
    }
  }

  def doMove(actions: List[Action], face: Face, start: Position): (Face, Position) =
    actions.foldLeft((face, start)) {
      case ((face, pos), action) =>
        val (newFace, dPos) = move(action, face)
        (newFace, pos + dPos)
    }

  def solutionPartA: String = {
    val (_, pos) = doMove(actions, 1, (0, 0))
    pos.norm.toString
  }

  def solutionPartB: String = ""

  run()
}
