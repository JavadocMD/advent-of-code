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
    import scala.collection.{mutable as m}
    val memory                         = m.Map.empty[String, Long]
    def memoize(k: String)(v: => Long) = memory.getOrElseUpdate(k, v)

    def recurse(curr: String): Long = memoize(curr):
      if curr == finish then 1 else nodes(curr).map(recurse).sum

    recurse(start)

  def part1(input: Input) = countPaths("you", "out", input)

  def part2(input: Input) =
    def paths(chain: List[String]): Long =
      chain.sliding(2).map(ns => countPaths(ns.head, ns.tail.head, input)).product

    List(
      List("svr", "dac", "fft", "out"),
      List("svr", "fft", "dac", "out"),
    ).map(paths).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
