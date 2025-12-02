package aoc2021

import scala.annotation.tailrec

import aoc.Day

object Day12 extends Day:
  def isSmall(node: String) = node.head.isLower
  type Graph = Map[String, List[String]]

  def parse(xs: Array[String]): Graph =
    xs.toList
      .flatMap({
        // edges entering "start" or leaving "end" are invalid
        case s"start-$b" => Seq("start" -> b)
        case s"$a-end"   => Seq(a -> "end")
        case s"$a-$b"    => Seq(a -> b, b -> a)
      })
      .groupMap(_._1)(_._2)

  type Path = List[String]
  val startPath: Path = List("start")

  def paths(graph: Graph, revisitAllowed: Boolean, path: Path = startPath): List[Path] =
    graph(path.head) flatMap {
      case "end"                  => List(path)
      case x if !isSmall(x)       => paths(graph, revisitAllowed, x :: path)
      case x if !path.contains(x) => paths(graph, revisitAllowed, x :: path)
      case x if revisitAllowed    => paths(graph, false, x :: path)
      case _                      => Nil
    }

  def part1(input: Graph) = paths(input, revisitAllowed = false).size

  def part2(input: Graph) = paths(input, revisitAllowed = true).size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
