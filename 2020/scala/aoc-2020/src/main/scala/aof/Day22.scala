package aof

object Day22 extends Day with App {

  type Deck = List[Int]

//  val day: String = "day23.txt"
  val day: String = "day22.txt"

  def parseDecks: (List[Int], List[Int]) = {
    assert(lines.head == "Player 1:")
    val (p1, rest) = lines.tail.span(!_.isBlank)
    assert(rest.tail.head == "Player 2:")
    (p1.map(_.toInt), rest.tail.tail.map(_.toInt))
  }

  val (p1, p2) = parseDecks

  def score(wd: List[Int]): Long =
    (1 to wd.length).reverse.zip(wd).foldLeft(0L) { case (acc, (l, r)) => acc + l * r }

  def solutionPartA: String = {
    def play(p1: List[Int], p2: List[Int]): List[Int] = (p1, p2) match {
      case (h1 :: t1, h2 :: t2) if h1 > h2 => play(t1 ::: List(h1, h2), t2)
      case (h1 :: t1, h2 :: t2) if h1 < h2 => play(t1, t2 ::: List(h2, h1))
      case (d, Nil)                        => d
      case (Nil, d)                        => d
    }
    score(play(p1, p2)).toString
  }

  def solutionPartB: String = {
    def play(p1: Deck, p2: Deck, visited: Set[(Deck, Deck)]): (Deck, Deck) = (p1, p2) match {
      case decks if visited.contains(decks) => play(p1 ::: p2, Nil, visited)
      case (h1 :: t1, h2 :: t2) if h1 <= t1.size && h2 <= t2.size =>
        play(t1, t2, Set.empty) match {
          case (_, Nil) => play(t1 ::: List(h1, h2), t2, visited + (p1 -> p2))
          case (Nil, _) => play(t1, t2 ::: List(h2, h1), visited + (p1 -> p2))
        }
      case (h1 :: t1, h2 :: t2) if h1 > h2 => play(t1 ::: List(h1, h2), t2, visited + (p1 -> p2))
      case (h1 :: t1, h2 :: t2) if h1 < h2 => play(t1, t2 ::: List(h2, h1), visited + (p1 -> p2))
      case (_, Nil)                        => (p1, p2)
      case (Nil, _)                        => (p1, p2)
    }
    def winning(decks: (Deck, Deck)): Deck = decks match {
      case (p1, Nil) => p1
      case (Nil, p2) => p2
    }
    score(winning(play(p1, p2, Set.empty))).toString
  }

  run()
}
