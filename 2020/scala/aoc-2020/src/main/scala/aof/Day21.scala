package aof

object Day21 extends Day with App {

  val day: String = "day21.txt"

  val foods = lines.map(Food.from)

  def ingredientToAllergenFreq(foods: List[Food]): Map[String, Map[String, Int]] =
    foods.foldLeft(Map.empty[String, Map[String, Int]]) { (acc, food) =>
      val pairs = for {
        x <- food.ingredients
        y <- food.allergens
      } yield (x, y)
      pairs.foldLeft(acc) {
        case (acc, (ing, all)) =>
          acc.updatedWith(ing) {
            case Some(counter) =>
              val newCounter = counter.updatedWith(all) {
                case Some(freq) => Some(freq + 1)
                case None       => Some(1)
              }
              Some(newCounter)
            case None => Some(Map(all -> 1))
          }
      }
    }

  def reversKeys[A, B, C](xs: Map[A, Map[B, C]]): Map[B, Map[A, C]] = {
    val zs = xs.map {
      case (key, ys) =>
        ys.toList.map {
          case (key2, z) => (key2, Map(key -> z))
        }.toMap
    }
    zs.foldLeft(Map.empty[B, Map[A, C]]) { (acc, x) =>
      x.toList.foldLeft(acc) {
        case (acc, (key, m)) =>
          acc.updatedWith(key) {
            case Some(value) => Some(value ++ m)
            case None        => Some(m)
          }
      }
    }
  }

  def go(allToIn: List[(String, Map[String, Int])], inWithNoAll: Set[String], inToAll: List[(String, String)]): (Set[String], List[(String, String)]) = allToIn match {
    case (h @ (allergen, inFreq)) :: t =>
      inFreq.toList.filter(in => inWithNoAll.contains(in._1)).sortBy(_._2)(Ordering[Int].reverse) match {
        case (ing, _) :: Nil                        => go(t, inWithNoAll - ing, (ing, allergen) :: inToAll)
        case (ing1, n1) :: (_, n2) :: _ if n1 != n2 => go(t, inWithNoAll - ing1, (ing1, allergen) :: inToAll)
        case (_, n1) :: (_, n2) :: _ if n1 == n2    => go(t ::: List(h), inWithNoAll, inToAll)
      }
    case Nil => (inWithNoAll, inToAll)
  }

  val inToAll = ingredientToAllergenFreq(foods)
  val allToIn = reversKeys(inToAll)
  val (inWithNoAll, inToOneAll) = go(allToIn.toList, inToAll.keys.toSet, List.empty)

  def solutionPartA: String = inWithNoAll.toList.map(in => foods.count(_.ingredients.contains(in))).sum.toString

  def solutionPartB: String = inToOneAll.sortBy(_._2).map(_._1).mkString(",")

  run()

  case class Food(ingredients: Set[String], allergens: Set[String])

  object Food {
    val r = """([^\(]+)\(contains ([^\)]+)\)""".r
    def from(s: String): Food = s match {
      case r(xs, ys) =>
        Food(
          xs.trim.split(' ').map(_.trim).toSet,
          ys.trim.split(',').map(_.trim).toSet
        )
    }
  }

}
