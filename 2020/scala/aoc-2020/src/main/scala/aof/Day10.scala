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

  def arrangements3(xs: List[Int]): Long = {
    @tailrec
    def go(todo: Set[(List[Int], List[Int])], current: (List[Int], List[Int]), acc: Long): Long = current match {
      case (Nil, _) if todo.isEmpty =>
        acc + 1
      case (Nil, _) =>
        go(todo.tail, todo.head, acc + 1)
      case (sorted, perm @ (curr :: _)) =>
        val (xs, restSorted) = sorted.span(s => s - curr <= 3)
        if (xs.isEmpty) {
          if (todo.nonEmpty) go(todo.tail, todo.head, acc) else acc
        } else if (xs.size == 1) {
          go(todo, (sorted.tail, sorted.head :: perm), acc)
        } else {
          val newTodo = ints(xs.tail).map(y => (y.tail ::: restSorted, y.head :: perm))
          go(todo ++ newTodo, (sorted.tail, sorted.head :: perm), acc)
        }
    }
    go(Set.empty, (xs.sorted, List(0)), 0)
  }

  def ints(xs: List[Int]): List[List[Int]] = xs.indices.map(i => xs.drop(i)).toList

  def solutionPartA: String = {
    val xs = adapters.sorted
    val cnt = countDiff(xs)
    (cnt(1) * cnt(3)).toString
  }

  def arrangements(xs: List[Int]): Long = {

    def go(xs: List[Int]): Int = xs match {
      case a :: b :: c :: t if c - a <= 3 => go(a :: c :: t) + go(b :: c :: t)
      case _ :: _ :: Nil                  => 1
      case _ :: t                         => go(t)
    }

    val sorted = xs.sorted
    go(0 :: sorted ::: List(sorted.max + 3))

  }

  def arrangements2(xs: List[Int]): Long = {

    def go(xs: List[Int], prev: Int): Long = xs match {
      case a :: b :: c :: d :: t if d - a <= 3 && a - prev < 3 => 1 + 6 * go(d :: t, c)
      case a :: b :: c :: d :: t if d - a <= 3                 => 1 + 3 * go(d :: t, c)
      //      case a :: b :: c :: t if c - a <= 3 && a - prev < 3 => 1 + 5 * go(c :: t, b)
      case a :: b :: c :: t if c - a <= 3 && a - prev <= 3 => 1 + 2 * go(c :: t, b)
      case a :: b :: c :: t if c - a <= 3                  => 1 + go(c :: t, b)
      case a :: b :: t if b - prev <= 3                    => 2 * go(b :: t, a)
      case a :: _ :: Nil                                   => 1
      case a :: t                                          => go(t, a)
      case Nil                                             => 0
    }

    val sorted = xs.sorted
    go(sorted ::: List(sorted.max + 3), 0)
  }

  def solutionPartB: String = {
    val sorted = adapters.sorted
    println(sorted)
    arrangements(adapters).toString
  }

  run()
}
