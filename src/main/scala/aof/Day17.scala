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
            (newFace, ms ++ nextMs, newPos, notVisited - newPos)
          }
        go(ys ::: t, acc)
      }
      case Nil => acc
    }

    val start: Vec = area.find { case (_, tile) => faces contains tile }.get._1
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

  def asciiEncode(xs: Seq[String]): Seq[Int] = xs.flatMap { x =>
    Coma :: x.toList.map(_.toInt)
  }.tail

  def encodeFunction(xs: Seq[String], routine: String, routines: Set[String] = Set("A", "B", "C"),
                     maxSize: Int = MaxFunctionSize): Set[(Seq[String], String)] = {
    def go(ys: String, xFun: String, acc: Set[String]): Set[String] = {
      if (ys.isEmpty) {
        acc + xFun
      } else {
        val comaIdx = ys.indexOf(',')
        val cnt = if (comaIdx == -1) ys.length else comaIdx max 1
        val c = ys.substring(0, cnt)
        if (routines contains c) {
          acc + xFun.substring(0, xFun.length - 1)
        } else {
          val nXFun = xFun + c
          go(ys.substring(cnt, ys.length), nXFun, if (ys.contains(nXFun) && c != ",") acc + nXFun else acc)
        }
      }
    }

    go(xs.dropWhile((routines + ",").contains).mkString(","), "", Set.empty)
      .filterNot(_.isEmpty)
      .filter(_.length <= maxSize)
      .flatMap { x =>
        val ys = x.split(',').toSeq
        //        Set(ys -> xs.mkString(",").replace(x, routine))
        replace(xs.mkString(","), x, routine).map(ys -> _)
      }
  }

  def replace(source: String, target: String, replacement: String): Set[String] = {

    def indexOf(src: String, target: String, from: Int = 0): List[Int] = {
      val idx = src.indexOf(target)
      if (idx == -1) {
        List.empty
      } else {
        val x = if (src.indexOf(target, idx + 1) == src.indexOf(target, idx + target.length)) target.length else 1
        (idx + from) :: indexOf(src.substring(idx + x), target, from + idx + x)
      }
    }

    val xs = indexOf(source, target)

    val ys = xs.zip(xs.drop(1)).filter { case (a, b) => b - a < target.length }

    if (ys.isEmpty) {
      Set(source.replace(target, replacement))
    } else {
      val zs = ys.flatMap { case (a, b) =>
        List(a, b).map(x => List(source.substring(0, x), source.substring(x)).map(_.replace(target, replacement)).mkString)
      }
      zs.toSet
    }
  }

  def encodeRoutines(xs: Seq[String], maxSize: Int = MaxFunctionSize,
                     routines: Set[String] = Set("A", "B", "C")): Set[(String, (Seq[String], Seq[String], Seq[String]))] = {
    val res = for {
      (aFun, ys) <- encodeFunction(xs, "A")
      (bFun, zs) <- encodeFunction(ys.split(',').toList, "B")
      (cFun, ws) <- encodeFunction(zs.split(',').toList, "C")
    } yield (ws, (aFun, bFun, cFun))

    res.filter {
      case (routine, _) =>
        routine.length <= maxSize && routine.split(',').toSet.removedAll(routines).isEmpty
    }
  }

  def solutionPartB: String = {
    val xs: Set[Vector[String]] = path(area).map(_.map(_.encode))

    def dbgF(path: Seq[String], x: (String, (Seq[String], Seq[String], Seq[String]))): String = x match {
      case (routine, (aF, bF, cF)) =>
        "path: [" + path.mkString(",") + "]" + "\n" + routine + " =>" + "\n  A: [" + aF.mkString(",") + "]" + "\n  B: [" + bF.mkString(",") + "]" + "\n  C: [" + cF.mkString(",") + "]"
    }

    val ys = xs.flatMap(x => encodeRoutines(x).map(x -> _))
      .map { case (x, (routines, (aF, bF, cF))) =>
        dbgF(x, (routines, (aF, bF, cF))) -> (asciiEncode(routines.split(',')), (asciiEncode(aF), asciiEncode(bF), asciiEncode(cF)))
      }

    val (dbg, (routines, (aF, bF, cF))) = ys.head

    println(dbg)
    println(s"routines: [${routines :+ NL}]")
    println(s"aF: [${aF :+ NL}]")
    println(s"bF: [${bF :+ NL}]")
    println(s"cF: [${cF :+ NL}]")
    println("video feed: " + Seq('n'.toInt, NL))

    val input: Seq[Int] = (routines :+ NL) ++ (aF :+ NL) ++ (bF :+ NL) ++ (cF :+ NL) ++ Seq('n'.toInt, NL)

    println("input: " + input.map(_.toString).mkString("[", ",", "]"))

    val mem = memory.toArray
    mem(0) = 2

    val c = IntComputer(mem).extendMemory(3 * 1024)
      .runInterpreter(input)

    println("output: " + c.output.map(_.toString).mkString("[", ",", "]"))

    "" + c.output.head

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
}
