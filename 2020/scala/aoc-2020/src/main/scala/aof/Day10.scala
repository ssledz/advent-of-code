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
    import collection.mutable
    val m: mutable.Map[List[Int], Long] = mutable.Map.empty[List[Int], Long]
    def memo[A, B](m: mutable.Map[A, B])(f: A => B): A => B = key => {
      if (m.contains(key)) {
        m(key)
      } else {
        val value = f(key)
        m.update(key, value)
        value
      }
    }
    def go(xs: List[Int]): Long = xs match {
      case a :: b :: c :: t if c - a <= 3 => memo(m)(go)(a :: c :: t) + memo(m)(go)(b :: c :: t)
      case _ :: _ :: Nil                  => 1
      case _ :: t                         => memo(m)(go)(t)
    }
    val sorted = xs.sorted
    go(0 :: sorted ::: List(sorted.max + 3))
  }

  def solutionPartB: String = arrangements(adapters).toString

  run()
}
