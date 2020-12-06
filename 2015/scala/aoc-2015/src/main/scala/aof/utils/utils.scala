package aof

import java.math.BigInteger
import java.security.MessageDigest

package object utils {

  def mergeMaps[K, V](m1: Map[K, V], m2: Map[K, V])(implicit m: Semigroup[V]): Map[K, V] =
    m1.foldLeft(m2) { case (acc, (key, value)) =>
      acc.updatedWith(key) {
        case Some(value2) => Some(m.combine(value, value2))
        case None => Some(value)
      }
    }

  def zipRight[A, B](xs: List[A], ys: List[B]): List[(A, Option[B])] = {
    def go(xs: List[A], ys: List[B], acc: List[(A, Option[B])]): List[(A, Option[B])] = (xs, ys) match {
      case (xh :: xt, yh :: yt) => go(xt, yt, (xh, Some(yh)) :: acc)
      case (Nil, _) => acc
      case (xh :: xt, Nil) => go(xt, Nil, (xh, None) :: acc)
    }

    go(xs, ys, List.empty).reverse
  }

  def windowed[A](xs: List[A], n: Int): (List[A], List[List[A]]) = {
    def go(xs: List[A], curr: List[A], acc: List[List[A]]): (List[A], List[List[A]]) = xs match {
      case h :: t if curr.size == n => go(t, List(h), curr.reverse :: acc)
      case h :: t => go(t, h :: curr, acc)
      case Nil if curr.size == n => (List.empty[A], (curr.reverse :: acc).reverse)
      case Nil => curr -> acc.reverse
    }

    go(xs, List.empty, List.empty)
  }

  def spanEven[A](xs: List[A]): (List[A], List[A]) = {
    val (ys, zs) = xs.foldLeft((List.empty[A], List.empty[A])) { case ((xs, ys), e) =>
      (ys, e :: xs)
    }
    if (xs.length % 2 == 0) (ys.reverse, zs.reverse) else (zs.reverse, ys.reverse)
  }

  def md5(s: String): String = {
    val digest = MessageDigest.getInstance("MD5").digest(s.getBytes("UTF-8"))
    val bigInt = new BigInteger(1, digest)
    val m = bigInt.toString(16)
    LazyList.continually("0").take(32 - m.length).mkString + m
  }

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

}
