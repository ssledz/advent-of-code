package aof

object Day02 extends Day with App {

  val day: String = "day02.txt"

  val xs = lines.map { l =>
    val arr = l.split(':')
    PasswordPolicy.from(arr(0)) -> arr(1).trim
  }

  def solutionPartA: String =
    xs.filter { case (policy, password) => policy.isValid(password) }.count(_ => true).toString

  def solutionPartB: String =
    xs.filter { case (policy, password) => policy.isValid2(password) }.count(_ => true).toString

  run()

  case class PasswordPolicy(min: Int, max: Int, letter: String) {
    def isValid(password: String): Boolean = {
      val cnt = password.count(letter == _.toString)
      cnt >= min && cnt <= max
    }

    def isValid2(password: String): Boolean = {
      val a = password.charAt(min - 1).toString == letter
      val b = password.charAt(max - 1).toString == letter
      a ^ b
    }
  }

  object PasswordPolicy {
    def from(s: String): PasswordPolicy = {
      val arr = s.trim.split(' ')
      val range = arr(0).split('-')
      PasswordPolicy(range(0).toInt, range(1).toInt, arr(1).trim)
    }
  }
}
