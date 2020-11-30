package aof

object Day11 extends Day {
  val day: String = "11"

  val Black = 0
  val White = 1

  def memory: Array[Long] = lines.head.split(',').map(_.toLong)

  def move(loc: (Int, Int), facing: Char): (Int, Int) = {
    val (x, y) = loc
    facing match {
      case '<' => (x - 1, y)
      case 'v' => (x, y + 1)
      case '>' => (x + 1, y)
      case '^' => (x, y - 1)
    }
  }

  def turn(facing: Char, direction: Int): Char = (facing, direction) match {
    case ('<', 0) => 'v'
    case ('v', 0) => '>'
    case ('>', 0) => '^'
    case ('^', 0) => '<'
    case ('<', 1) => '^'
    case ('v', 1) => '<'
    case ('>', 1) => 'v'
    case ('^', 1) => '>'
  }

  def travers(start: (Int, Int), facing: Char, panels: Set[Panel] = Set.empty): Set[Panel] = {
    def go(c: IntComputer, loc: (Int, Int), facing: Char, visited: Set[Panel]): Set[Panel] = {
      if (c.waitingInput) {
        val currentPanel = visited.find(_.loc == loc).getOrElse(Panel(loc))
        val (direction :: color :: Nil, newC) = c.runInterpreter(List(currentPanel.color)).readOutput
        val newFacing = turn(facing, direction.toInt)
        val paintedPanel = currentPanel.paint(color)
        go(newC, move(loc, newFacing), newFacing, visited.filterNot(_.loc == loc) + paintedPanel)
      } else visited
    }

    go(new IntComputer(memory).extendMemory().runInterpreter(List.empty), start, '^', panels)
  }

  def solutionPartA: String = {
    "" + travers((0, 0), '^').filter(_.painted).size
  }

  def draw(ps: Set[Panel]): String = {
    val xs = ps.map(_.loc._1).toList.sorted
    val ys = ps.map(_.loc._2).toList.sorted

    val xr = xs.min - 1 to xs.max + 1
    val yr = ys.min - 1 to ys.max + 1

    val zs = for {
      y <- yr
      x <- xr
    } yield {
      val c = ps.find(_.loc == (x, y)).map(_.color).getOrElse(Black)
      if (c == Black) '.' else '#'
    }

    zs.sliding(xr.length, xr.length).map(_.mkString).mkString("\n")

  }

  def solutionPartB: String = {

    val xs = travers((0, 0), '^', Set(Panel((0, 0), White)))

    "\n" + draw(xs)
  }

  case class Panel(loc: (Int, Int), color: Int = Black, painted: Boolean = false) {
    def paint(c: Long): Panel = this.copy(color = c.toInt, painted = true)
  }

}

object Day11App extends App {
  println("SolutionPartA: " + Day11.solutionPartA)
  println("SolutionPartB: " + Day11.solutionPartB)
}