package aof

object Day06 extends Day with App {

  val day: String = "day06.txt"

  val commands = lines.map(Command.from)

  val h = 1000
  val w = 1000

  def grid: Array[Array[Int]] = Array.ofDim[Int](h, w)

  def update(grid: Array[Array[Int]], x: Int, y: Int)(f: Int => Int): Unit = set(grid)(x, y, f(get(grid)(x, y)))

  def get(grid: Array[Array[Int]])(x: Int, y: Int): Int = grid(y)(x)

  def set(grid: Array[Array[Int]])(x: Int, y: Int, value: Int): Unit = grid(y)(x) = value

  def runCommands(grid: Array[Array[Int]], cmds: List[Command])(f: Command => Int => Int): Unit = {
    cmds.foreach { cmd =>
      for {
        x <- cmd.start._1 to cmd.end._1
        y <- cmd.start._2 to cmd.end._2
      } yield {
        update(grid, x, y)(f(cmd))
      }
    }
  }

  def solutionPartA: String = {
    val localGrid = grid
    runCommands(localGrid, commands)(_.fun)
    localGrid.map(_.sum).sum.toString
  }

  def solutionPartB: String = {
    val localGrid = grid
    runCommands(localGrid, commands)(_.fun2)
    localGrid.map(_.sum).sum.toString
  }

  run()

  def drawGrid(grid: Array[Array[Int]], w: Int = w, h: Int = h): String = {
    val rows = (0 until h).map { y =>
      (0 until w).map(x => get(grid)(x, y).toString).mkString
    }
    rows.mkString("\n")
  }

  case class Command(action: (String, Option[String]), start: (Int, Int), end: (Int, Int)) {

    def fun: Int => Int = action match {
      case ("turn", Some("off")) => _ => 0
      case ("turn", Some("on")) => _ => 1
      case ("toggle", None) => {
        case 0 => 1
        case 1 => 0
      }
    }

    def fun2: Int => Int = action match {
      case ("turn", Some("off")) => x => Math.max(x - 1, 0)
      case ("turn", Some("on")) => x => x + 1
      case ("toggle", None) => _ + 2
    }

  }

  def parsePoint(s: String): (Int, Int) = {
    val arr = s.split(',')
    (arr(0).toInt, arr(1).toInt)
  }

  object Command {
    def from(s: String): Command = {
      val arr = s.split(' ')
      arr(0) match {
        case "turn" => Command((arr(0), Some(arr(1))), parsePoint(arr(2)), parsePoint(arr(4)))
        case "toggle" => Command((arr(0), None), parsePoint(arr(1)), parsePoint(arr(3)))
      }
    }
  }

}
