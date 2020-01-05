package aof

import aof.Day17.{solutionPartA, solutionPartB}

object Day17 extends Day {

  val day: String = "17"

  val memory: List[Long] = lines.head.split(',').map(_.toLong).toList

  val MaxFunctionSize = 20
  val NL = 10
  val Coma = ','.toInt
  val Scaffold = 35 // #
  val Intersection = 'O'.toInt
  val RobotFaces = Map(('^'.toInt, Vec(0, -1)), ('v'.toInt, Vec(0, 1)), ('<'.toInt, Vec(-1, 0)), ('>'.toInt, Vec(1, 0)))

  def buildMap(xs: List[Long]): Map[Vec, Int] = xs.map(_.toInt)
    .foldLeft((Vec.Zero, Map.empty[Vec, Int])) { case ((vec@Vec(x, y), as), a) =>
      if (a == NL) {
        (Vec(0, y + 1), as)
      } else {
        (Vec(x + 1, y), as + (vec -> a))
      }
    }._2

  def minMax(area: Map[Vec, Int]): (Vec, Vec) = area.toList
    .foldLeft((Vec.MaxValue, Vec.MinValue)) { case ((min, max), (pos, _)) =>
      (min min pos, max max pos)
    }

  def writeArea(area: Map[Vec, Int], render: Option[Int] => String): String = {
    val (min, max) = minMax(area)
    (min.y to max.y).toList.map { y =>
      (min.x to max.x).toList.map(x => render(area.get(Vec(x, y)))).mkString
    }.mkString("\n")
  }

  val Mask = List(Vec(0, 0), Vec(-1, 0), Vec(1, 0), Vec(0, -1), Vec(0, 1))

  val IntersectionValue = Mask.length * Scaffold

  def scaffoldInter(area: Map[Vec, Int]): Set[Vec] = {

    def opF(x: Vec): Int = Mask.map(_ + x).map(area.get).flatten.sum

    val (min, max) = minMax(area)

    val xs = for {
      x <- (min.x + 1) to (max.x - 1)
      y <- (min.y + 1) to (max.y - 1)
    } yield Vec(x, y)

    xs.map(vec => vec -> opF(vec)).filter(_._2 == IntersectionValue).map(_._1).toSet

  }

  val area = {
    val output = IntComputer(memory.toArray).extendMemory(3 * 1024)
      .runInterpreter(List.empty).output
    buildMap(output.reverse)
  }

  def solutionPartA: String = {

    val renderer: Option[Int] => String = {
      case Some(value) => value.toChar.toString
      case _ => "?"
    }

    val ints = scaffoldInter(area)

    println(writeArea(area ++ ints.map(_ -> Intersection), renderer))

    "" + ints.map { case Vec(x, y) => x * y }.sum
  }

  sealed trait MovF {
    def encode: String = this match {
      case Forward(x) => x.toString
      case Turn(x) => x.toString
    }
  }

  case class Forward(x: Int) extends MovF

  case class Turn(x: Char) extends MovF

  def path(area: Map[Vec, Int]): Set[Vector[MovF]] = {

    val ints = scaffoldInter(area)

    val faces = RobotFaces.keys.toSet

    //    val rotations = Vector('^','>', 'v', '<')

    def rotate(sf: Int, ef: Int): Option[MovF] = (sf.toChar, ef.toChar) match {
      case ('^', '>') => Some(Turn('R'))
      case ('^', '<') => Some(Turn('L'))
      case ('v', '>') => Some(Turn('L'))
      case ('v', '<') => Some(Turn('R'))
      case ('<', '^') => Some(Turn('R'))
      case ('<', 'v') => Some(Turn('L'))
      case ('>', '^') => Some(Turn('L'))
      case ('>', 'v') => Some(Turn('R'))
      case _ => None
    }

    def isScaffold(pos: Vec): Boolean = area.get(pos).exists(_ == Scaffold)

    def movesFrom(p: Vec, face: Int): List[(Int, Vector[MovF], Vec)] = {

      val otherFaces = faces - face

      val maybeForward = {
        val newP = p + RobotFaces(face)
        if (isScaffold(newP)) List((face, Vector(Forward(1)), newP)) else List.empty
      }

      maybeForward ::: otherFaces.toList.flatMap(nFace => rotate(face, nFace).map(nFace -> _))
        .map { case (nFace, turn) => (nFace, Vector(turn, Forward(1)), p + RobotFaces(nFace)) }
        .filter { case (_, _, pos) => isScaffold(pos) }
    }

    def go(xs: List[(Int, Vector[MovF], Vec, Set[Vec])], acc: List[Vector[MovF]] = List.empty): List[Vector[MovF]] = xs match {
      case (_, ms, _, notVisited) :: t if notVisited.isEmpty => go(t, ms :: acc)
      case (face, ms, pos, notVisited) :: t => {
        val ys = movesFrom(pos, face)
          .filter { case (_, _, newPos) => (ints contains newPos) || (notVisited contains newPos) }
          .map { case (newFace, nextMs, newPos) =>
            //            if(notVisited.size <= 2 ) {
            //              println("(newFace, pos, newPos, notVisited - pos): " + (newFace, pos, newPos, notVisited - newPos))
            //              println("min notVisited.size: " + notVisited.size + " => " + notVisited)
            //            }
            (newFace, ms ++ nextMs, newPos, notVisited - newPos)
          }
        go(ys ::: t, acc)
      }
      case Nil => acc
    }

    val start: Vec = area.find { case (_, tile) => faces contains tile }.get._1
    //    println("start: " + start)
    //    println("face: " + area(start))
    //    println("movesFrom: " + movesFrom(start, area(start)))
    //    println("movesFrom: " + movesFrom(Vec(3, 0), '<'.toInt))
    //    println("movesFrom: " + movesFrom(Vec(2, 0), '<'.toInt))
    //    println("movesFrom: " + movesFrom(Vec(1, 0), '<'.toInt))
    //    println("movesFrom(0, 0): " + movesFrom(Vec(0, 0), '<'.toInt))
    //    println("movesFrom(0, 1): " + movesFrom(Vec(0, 1), 'v'.toInt))
    //    println("movesFrom(22, 4): " + movesFrom(Vec(22, 4), '>'.toInt))
    //    println("movesFrom(23, 4): " + movesFrom(Vec(23, 4), '>'.toInt))
    //    println("movesFrom(24, 4): " + movesFrom(Vec(24, 4), '>'.toInt))
    val res = go(List((area(start), Vector.empty, start, area.filter { case (_, tile) => tile == Scaffold }.keys.toSet - start)))
    res.toSet.map { moves: Vector[MovF] =>
      moves.foldLeft(Vector.empty[MovF]) { case (acc, move) =>
        (acc.lastOption, move) match {
          case (Some(Forward(value)), Forward(inc)) => acc.init :+ Forward(value + inc)
          case _ => acc :+ move
        }
      }
    }
  }

  def asciiEncode(xs: Seq[String], nl: Seq[Int] = Seq(NL)): Seq[Int] = xs.flatMap { x =>
    Coma :: x.toList.map(_.toInt)
  }.tail ++ nl

  def encodeFunctions(xs: Seq[Int]): List[(Seq[Int], Seq[Int], Seq[Int])] = ???

  def encodeRoutines(a: Seq[Int], b: Seq[Int], c: Seq[Int], xs: Seq[Int]): Seq[Int] = ???

  def encodePath(xs: Seq[String]): List[(Seq[Int], (Seq[Int], Seq[Int], Seq[Int]))] = {
    val ys = asciiEncode(xs, Seq.empty)
    for {
      (aF, bF, cF) <- encodeFunctions(ys)
    } yield (encodeRoutines(aF, bF, cF, ys), (aF, bF, cF))
  }

  def solutionPartB: String = {
    val xs: Set[Vector[String]] = path(area).map(_.map(_.encode))

    println("path.head: " + xs.head.mkString(","))

    val ys = xs.flatMap(encodePath).filter { case (routines, (aF, bF, cF)) =>
      List(routines, aF, bF, cF).forall(_.size <= MaxFunctionSize)
    }

    val (routines, (aF, bF, cF)) = ys.head

    val input: Seq[Int] = (routines :+ NL) ++ (aF :+ NL) ++ (bF :+ NL) ++ (cF :+ NL) ++ Seq('n'.toInt, NL)

    val c = IntComputer(memory.toArray).extendMemory(3 * 1024)
      .runInterpreter(input)

    println("output: " + c.output)

    "\n" + c.output.head
  }

  case class Vec(x: Int, y: Int) {
    def +(other: Vec): Vec = Vec(x + other.x, y + other.y)

    def min(other: Vec): Vec = Vec(x min other.x, y min other.y)

    def max(other: Vec): Vec = Vec(x max other.x, y max other.y)

    def manhattanLen: Int = x + y
  }

  object Vec {
    val MaxValue = Vec(Int.MaxValue, Int.MaxValue)
    val MinValue = Vec(Int.MinValue, Int.MinValue)
    val Zero = Vec(0, 0)
  }

}

object Day17App extends App {
  println("SolutionPartA: " + solutionPartA)
  println("SolutionPartB: " + solutionPartB)
  //  println(Math.log10(1))
  //  println(Math.log10(4).toInt + 1)
  //  println(Math.log10(10).toInt + 1)
}
