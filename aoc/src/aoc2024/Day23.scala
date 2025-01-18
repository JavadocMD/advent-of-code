package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day23 extends Day:

  type Input = List[(String, String)]

  def parse(xs: List[String]): Input =
    xs.map:
      case s"$a-$b" => (a, b)

  def connections(input: Input): Map[String, Set[String]] =
    val m = scala.collection.mutable.Map.empty[String, Set[String]]
    def add(a: String, b: String): Unit =
      val aSet = m.getOrElse(a, Set.empty)
      val bSet = m.getOrElse(b, Set.empty)
      m.update(a, aSet + b)
      m.update(b, bSet + a)
    input.toIterator.foreach(add)
    m.toMap

  def part1(input: Input) =
    val conns = connections(input)
    val triples = for
      a <- conns.keySet.filter(_.startsWith("t"))
      b <- conns(a)
      c <- conns(a)
      if b != c && conns(b).contains(c)
    yield List(a, b, c).sorted
    triples.toSet.size

  def findGroups(conns: Map[String, Set[String]]): Set[Set[String]] =
    @tailrec
    def recurse(unexplored: List[String], group: Set[String]): Set[String] =
      unexplored match
        case Nil => group
        case head :: tail =>
          if group.forall(x => conns(x).contains(head)) then recurse(tail, group + head)
          else recurse(tail, group)

    conns
      .map: (x, cxs) =>
        recurse(cxs.toList, Set(x))
      .toSet

  def part2(input: Input) =
    val cs = connections(input).map:
      case (key, set) => (key, set + key)
    findGroups(cs).maxBy(_.size).toList.sorted.mkString(",")

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
