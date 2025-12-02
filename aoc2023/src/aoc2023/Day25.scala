package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day25 extends Day:

  type EdgeMap = Map[String, Set[(String, String)]]

  case class Input(edges: EdgeMap):
    val nodes = edges.keySet

  def parse(xs: List[String]): Input =
    var conns = Map.empty[String, Set[(String, String)]].withDefault(_ => Set.empty)
    for
      case s"$left: $rest" <- xs
      right <- rest.split(" ")
    do
      conns = conns.updated(left, conns(left) + ((left, right)))
      conns = conns.updated(right, conns(right) + ((left, right)))
    Input(conns)

  def partition(group: Set[String], edges: EdgeMap): Set[String] =
    // Get the set of edges leading out of our group.
    val boundaryEdges = group
      .map(edges)
      .flatten
      .filterNot:
        case (a, b) => group.contains(a) && group.contains(b)

    // Stop growing the group when there are exactly three boundary edges (the ones we'd cut).
    if boundaryEdges.size == 3 then group
    else
      // Otherwise grow the group. We want to select the most-connected node,
      // or the one that comes up the most in the boundary edges.
      // Flatten out the edges to a list of nodes,
      // keep duplicates,
      // drop the ones that are in the group,
      // then group and count.
      val connectedness = boundaryEdges.toList
        .flatMap:
          case (a, b) => a :: b :: Nil
        .filterNot(group.contains)
        .groupMapReduce(identity)(_ => 1)(_ + _)

      val next = connectedness.maxBy(_._2)._1
      partition(group + next, edges)

  def part1(input: Input) =
    // I think we can choose any node to start the group.
    val p1 = partition(Set(input.nodes.head), input.edges).size
    val p2 = input.nodes.size - p1
    p1 * p2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
