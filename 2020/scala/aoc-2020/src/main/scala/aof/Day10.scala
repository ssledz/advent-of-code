package aof

object Day10 extends Day with App {

  val day: String = "day10.txt"

  val adapters = lines.map(_.toInt)

  def countDiff(xs: List[Int]): Map[Int, Int] = {
    def inc(k: Int, m: Map[Int, Int]): Map[Int, Int] = m.updatedWith(k)(x => x.map(y => y + 1).orElse(Some(1)))
    val (_, cnt) = xs.foldLeft((0, Map.empty[Int, Int])) {
      case ((s, acc), x) =>
        val diff = x - s
        (x, inc(diff, acc))
    }
    inc(3, cnt)
  }

  def solutionPartA: String = {
    val xs = adapters.sorted
    val cnt = countDiff(xs)
    (cnt(1) * cnt(3)).toString
  }

  def solutionPartB: String = {

    val sorted = adapters.sorted

//    println(sorted)

    def go(todo: List[(List[Int], List[Int])], current: (List[Int], List[Int]), acc: Set[List[Int]]): Set[List[Int]] = current match {
      case (Nil, arr) if todo.isEmpty =>
//        println(arr.reverse)
        acc + arr
      case (Nil, arr)                 =>
//        println(arr.reverse)
        go(todo.tail, todo.head, acc + arr)
      case (sorted, perm @ (curr :: _)) =>
        val (xs, restSorted) = sorted.span(s => s - curr <= 3)
        if (xs.isEmpty) {
          if (todo.nonEmpty) go(todo.tail, todo.head, acc) else acc
        } else if (xs.size == 1) {
          go(todo, (sorted.tail, sorted.head :: perm), acc)
        } else {
//          val newTodo = xs.tail.map(x => (xs.filterNot(_ == x) ::: restSorted, x :: perm))
//          val newTodo = xs.tail.zipWithIndex.map{ case (x, i) => (xs.drop(i) ::: restSorted, x :: perm) }
          val newTodo = ints(xs).map(x => (x.tail ::: restSorted, x.head :: perm))
//          println()
//          println(s"($curr)")
//          println("current\t" + current)
//          println("nextCurrent\t" + (sorted.tail, sorted.head :: perm))
//          println("newTodo\t" + newTodo)
//          println("todo\t" + todo)
          go(newTodo ::: todo, (sorted.tail, sorted.head :: perm), acc)
        }
    }

    val arr = go(List.empty, (sorted, List(0)), Set.empty)

    arr.size.toString
  }

  def ints(xs: List[Int]) = (0 until xs.size).map(i => xs.drop(i)).toList

  run()
}
