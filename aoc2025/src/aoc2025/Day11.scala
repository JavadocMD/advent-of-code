package aoc2025

import scala.annotation.tailrec
import aoc.Day

object Day11 extends Day:

  type Input = Map[String, List[String]]

  def parse(xs: List[String]): Input =
    Map.from(
      xs.map:
        case s"$name: $outputs" => name -> outputs.split(" ").toList
    ) + ("out" -> Nil)

  def countPaths(start: String, finish: String, nodes: Map[String, List[String]]): Long =
    @tailrec
    def recurse(open: List[String], acc: Long = 0L): Long =
      open match
        case Nil                            => acc
        case head :: tail if head == finish => recurse(tail, acc + 1)
        case head :: tail                   => recurse(nodes(head) ::: tail, acc)

    recurse(start :: Nil)

  def part1(input: Input) = countPaths("you", "out", input)

  def reachable(from: String, nodes: Map[String, List[String]]): Set[String] =
    // Which nodes can be reached starting from the given node?
    // If you reverse the nodes mapping, you can compute the opposite:
    // which nodes can reach the given node.
    @tailrec
    def recurse(open: Vector[String], acc: Set[String] = Set.empty): Set[String] =
      if open.isEmpty then acc
      else
        val next = nodes(open.head).filterNot(acc.contains)
        recurse(open.tail ++ next, acc ++ next)

    recurse(Vector(from), Set(from))

  def reverseNodes(nodes: Map[String, List[String]]): Map[String, List[String]] =
    Map.from(
      for name <- nodes.keys
      yield name -> nodes.keys.filter(x => nodes(x).contains(name)).toList
    )

  def filterNodes(nodes: Map[String, List[String]], keep: Set[String]): Map[String, List[String]] =
    // Filter nodes in both keys and values to just the keeper set.
    Map.from(
      for
        (k, vs) <- nodes
        if keep.contains(k)
      yield k -> vs.filter(keep.contains)
    )

  def countPaths2(start: String, finish: String, nodes: Map[String, List[String]]): Long =
    // I'm trying to trim these problems down to the solvable realm by
    // filtering out nodes which aren't relevant to this search.
    // To that end, only consider nodes which are both reachable from the start and
    // can themselves reach the finish.
    val reach = reachable(start, nodes) & reachable(finish, reverseNodes(nodes))
    if reach.size == 0 then 0L
    else countPaths(start, finish, filterNodes(nodes, reach))

  def part2(input: Input) =
    // In this part I was only saved by the fact that there is no path from dac -> fft
    // and that the paths from fft -> dac were small enough to compute
    // (after filtering for the intersection of nodes reachable from both endpoints).
    // This feels like kind of a cheating solution, but I guess that's where we're at.

    // everyPath2("svr", "dac", input), // (takes too long to compute)
    // everyPath2("dac", "fft", input), // 0
    // everyPath2("fft", "out", input), // (takes too long to compute)

    Seq(
      countPaths2("svr", "fft", input), // 17396
      countPaths2("fft", "dac", input), // 3890598
      countPaths2("dac", "out", input), // 4530
    ).product

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
