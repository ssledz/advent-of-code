package aof

package object utils {

  def mergeMaps[K, V](m1: Map[K, V], m2: Map[K, V])(implicit m: Semigroup[V]): Map[K, V] =
    m1.foldLeft(m2) { case (acc, (key, value)) =>
      acc.updatedWith(key) {
        case Some(value2) => Some(m.combine(value, value2))
        case None => Some(value)
      }
    }

  def spanEven[A](xs: List[A]): (List[A], List[A]) = {
    val (ys, zs) = xs.foldLeft((List.empty[A], List.empty[A])) { case ((xs, ys), e) =>
      (ys, e :: xs)
    }
    if(xs.length % 2 == 0) (ys.reverse, zs.reverse) else (zs.reverse, ys.reverse)
  }

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

}
