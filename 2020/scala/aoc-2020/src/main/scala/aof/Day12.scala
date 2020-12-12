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

  def rotateWaypoint(p: Position, degree: Int, dir: Char): Position = (dir, degree, p) match {
    case ('R', 90, (x, y)) => (-y, x)
    case ('L', 90, (x, y)) => (y, -x)
    case _                 => rotateWaypoint(rotateWaypoint(p, 90, dir), degree - 90, dir)
  }

  def move2(a: Action, waypoint: Position): (Position, Vector) = {
    val (dir, r) = a
    dir match {
      case 'N'       => waypoint + (0, -1) * r -> (0, 0)
      case 'S'       => waypoint + (0, 1) * r -> (0, 0)
      case 'W'       => waypoint + (-1, 0) * r -> (0, 0)
      case 'E'       => waypoint + (1, 0) * r -> (0, 0)
      case 'F'       => waypoint -> waypoint * r
      case 'L' | 'R' => rotateWaypoint(waypoint, r, dir) -> (0, 0)
    }
  }

  def doMove[A](actions: List[Action], face: A, start: Position, move: (Action, A) => (A, Vector)): Position =
    actions
      .foldLeft((face, start)) {
        case ((face, pos), action) =>
          val (newFace, dPos) = move(action, face)
          (newFace, pos + dPos)
      }
      ._2

  def solutionPartA: String = doMove(actions, 1, (0, 0), move).norm.toString

  def solutionPartB: String = doMove(actions, (10, -1), (0, 0), move2).norm.toString

  run()
}
