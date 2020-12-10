package aof

import scala.annotation.tailrec

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

  @tailrec
  def countArr(todo: Set[(List[Int], List[Int])], current: (List[Int], List[Int]), acc: Int): Int = current match {
    case (Nil, arr) if todo.isEmpty =>
      //        println(arr.reverse)
      acc + 1
    case (Nil, arr) =>
      //        println(arr.reverse)
      countArr(todo.tail, todo.head, acc + 1)
    case (sorted, perm @ (curr :: _)) =>
      val (xs, restSorted) = sorted.span(s => s - curr <= 3)
      if (xs.isEmpty) {
        if (todo.nonEmpty) countArr(todo.tail, todo.head, acc) else acc
      } else if (xs.size == 1) {
        countArr(todo, (sorted.tail, sorted.head :: perm), acc)
      } else {
        val newTodo = ints(xs.tail).map(y => (y.tail ::: restSorted, y.head :: perm))
        //          println()
        //          println(s"($curr)")
        //          println("current\t" + current)
        //          println("nextCurrent\t" + (sorted.tail, sorted.head :: perm))
        //          println("newTodo\t" + newTodo)
        //          println("todo\t" + todo)
        //          println("acc\t" + acc)
        countArr(todo ++ newTodo, (sorted.tail, sorted.head :: perm), acc)
      }
  }

  def ints(xs: List[Int]): List[List[Int]] = xs.indices.map(i => xs.drop(i)).toList

  def solutionPartA: String = {
    val xs = adapters.sorted
    val cnt = countDiff(xs)
    (cnt(1) * cnt(3)).toString
  }

  def solutionPartB: String = {
    val sorted = adapters.sorted
    val cnt = countArr(Set.empty, (sorted, List(0)), 0)
    cnt.toString
  }

  run()
}
