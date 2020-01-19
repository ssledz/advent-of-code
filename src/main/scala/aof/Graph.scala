package aof

import aof.Graph.{Edge, Label, SearchState, StringLabel}

import scala.annotation.tailrec

case class Graph(edges: Vector[Vector[Edge]],
                 vLabel: Map[Int, Label] = Map.empty,
                 eLabel: Map[(Int, Int), Label] = Map.empty) {

  self =>

  def withVLabel(vLabel: Map[Int, Label]): Graph = self.copy(vLabel = vLabel)

  def withELabel(eLabel: Map[(Int, Int), Label]): Graph = self.copy(eLabel = eLabel)
}

object Graph {

  def apply(nVertices: Int, edges: (Int, Edge)*): Graph = {
    val zero = (0 until nVertices).map(_ => Vector.empty[Edge]).toVector
    val v2es = edges.groupBy(_._1).view.mapValues(_.map(_._2).toSet.toVector).toSeq
    Graph(v2es.foldLeft(zero) { case (acc, (v, edges)) => acc.updated(v, edges) })
  }

  def bfs(g: Graph, start: Int): Vector[Int] = bfs(g, start, (_, s) => s, (_, s) => s)._1

  def bfs(g: Graph, start: Int, visitEdge: ((Int, Edge), SearchState) => SearchState,
          visitVertex: (Int, SearchState) => SearchState): (Vector[Int], SearchState) = {

    val nVs = g.edges.size
    val bZero = Vector.fill(nVs)(false)

    @tailrec
    def go(vs: Vector[Int], processed: Vector[Boolean], discovered: Vector[Boolean], parent: Vector[Int],
           state: SearchState): (Vector[Int], SearchState) = vs match {
      case v +: t => {
        val s = visitVertex(v, state)
        val (newVs, newDiscovered, newParent, newState) = g.edges(v)
          .filterNot(e => discovered(e.v))
          .foldLeft((t, discovered, parent, s)) { case ((vs, discovered, parent, state), e) =>
            val newState = if (!processed(e.v)) visitEdge(v -> e, state) else state
            (e.v +: vs, discovered.updated(e.v, true), parent.updated(e.v, v), newState)
          }
        go(newVs, processed.updated(v, true), newDiscovered, newParent, newState)
      }
      case _ => parent -> state
    }

    go(Vector(start), bZero, bZero.updated(start, true), Vector.fill(nVs)(-1), EmptySearchState)
  }


  def show(g: Graph): String = {
    g.edges.zipWithIndex.map { case (edges, v) =>
      val vLabel = g.vLabel.get(v).map(l => s" (label : $l)").getOrElse("")
      edges.map { e =>
        val eLabel = g.eLabel.get((v, e.v)).map(l => s", label: $l").getOrElse("")
        val weight = s"weight: ${e.weight}"
        s"$v$vLabel => ${e.v} ($weight$eLabel)"
      }.mkString("\n")
    }.filterNot(_.isEmpty).mkString("\n")
  }

  trait Label

  case class StringLabel(s: String) extends Label

  case class Edge(v: Int, weight: Int = 1)

  trait SearchState

  object EmptySearchState extends SearchState

}

object TestApp extends App {


  val g = Graph(10, 0 -> Edge(0), 0 -> Edge(1), 1 -> Edge(2), 2 -> Edge(3), 2 -> Edge(4), 3 -> Edge(4))
    .withVLabel(Map(0 -> StringLabel("start")))
  println(Graph.show(g))
  println(Graph.bfs(g, 0))

  def visitEdge(edge: (Int, Edge), state: SearchState): SearchState = {
    println(s"visiting edge: $edge")
    state
  }

  def visitVertex(v: Int, state: SearchState): SearchState = {
    println(s"visiting vertex: $v")
    state
  }

  println(Graph.bfs(g, 0, visitEdge, visitVertex))

}