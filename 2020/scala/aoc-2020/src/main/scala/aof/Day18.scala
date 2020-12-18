package aof

object Day18 extends Day with App {

  val day: String = "day18.txt"

  def parseTokens(in: String): List[Token] = {
    val ops = Set('+', '*')
    def go(xs: List[Char], acc: List[Token]): List[Token] = xs match {
      case d :: t if d.isDigit         => go(t, Number(d.toString.toInt) :: acc)
      case op :: t if ops.contains(op) => go(t, Operator(op) :: acc)
      case '(' :: t                    => go(t, LeftParen :: acc)
      case ')' :: t                    => go(t, RightParen :: acc)
      case ' ' :: t                    => go(t, acc)
      case Nil                         => acc
    }
    go(in.toList, List.empty).reverse
  }

  def evalOp(a: Long, b: Long, op: Char): Long = if (op == '+') a + b else a * b

  def eval(in: String): Long = {
    def go(xs: List[Token], acc: Long, op: Char): (List[Token], Long) = xs match {
      case Number(n) :: t    => go(t, evalOp(acc, n, op), op)
      case Operator(op) :: t => go(t, acc, op)
      case LeftParen :: t =>
        val (rest, value) = go(t, 0, '+')
        go(rest, evalOp(acc, value, op), op)
      case RightParen :: t => (t, acc)
      case Nil             => (Nil, acc)
    }
    go(parseTokens(in), 0, '+')._2
  }

  def eval2(in: String): Long = {
    def go(xs: List[Token], stack: List[Long], ops: List[Char]): (List[Token], Long) = (xs, stack) match {
      case (Number(n) :: t, ys) if ops.isEmpty            => go(t, n :: ys, ops)
      case (Number(n) :: t, a :: ys) if ops.head == '+'   => go(t, (a + n) :: ys, ops.tail)
      case (Number(n) :: t, _)                            => go(t, n :: stack, ops)
      case (Operator(op) :: t, a :: b :: ys) if op == '*' => go(t, (a * b) :: ys, ops)
      case (Operator(op) :: t, _)                         => go(t, stack, op :: ops)
      case (LeftParen :: t, a :: ys) =>
        val (rest, value) = go(t, List(0), List('+'))
        if (ops.head == '*') go(rest, value :: stack, ops)
        else go(rest, evalOp(a, value, ops.head) :: ys, ops.tail)
      case (RightParen :: t, acc :: Nil)    => (t, acc)
      case (RightParen :: t, a :: b :: Nil) => (t, evalOp(a, b, ops.head))
      case (Nil, acc :: Nil)                => (Nil, acc)
      case (Nil, a :: b :: Nil)             => (Nil, evalOp(a, b, ops.head))
    }
    go(parseTokens(in), List(0), List('+'))._2
  }

  def solutionPartA: String = lines.map(eval).sum.toString

  def solutionPartB: String = lines.map(eval2).sum.toString

  run()

  sealed trait Token
  case class Number(value: Int) extends Token
  case class Operator(op: Char) extends Token
  case object LeftParen extends Token
  case object RightParen extends Token

}
