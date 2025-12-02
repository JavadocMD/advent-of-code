package aoc2024

import scala.annotation.tailrec
import aoc.Day
import scala.collection.View

object Day22 extends Day:

  type Input = List[Long]

  def parse(xs: List[String]): Input = xs.map(_.toLong)

  inline def s1(n: Long)        = ((n << 6) ^ n) & 0xffffff
  inline def s2(n: Long)        = ((n >> 5) ^ n) & 0xffffff
  inline def s3(n: Long)        = ((n << 11) ^ n) & 0xffffff
  def nextSecret(n: Long): Long = s3(s2(s1(n)))

  def part1(input: Input) =
    input
      .map: n =>
        View.iterate(n, 2001)(nextSecret).last
      .sum

  case class Monkey(prices: Vector[Int], deltas: Vector[Int]):
    val scoreOf = Map
      .from(deltas.sliding(4).zipWithIndex.toList.reverseIterator)
      .mapValues(x => prices(x + 3))

  def part2(input: Input) =
    import scala.collection.parallel.CollectionConverters._

    val monkeys = input.map: n =>
      val prices = Iterator.iterate(n, 2001)(nextSecret).map(x => (x % 10).toInt).toVector
      val deltas = prices
        .sliding(2)
        .map:
          case Seq(a, b) => b - a
          case _         => throw Exception("unhandled case")
        .toVector
      Monkey(prices.drop(1), deltas)

    def score(seq: Vector[Int]): Int = monkeys.iterator.map(_.scoreOf.getOrElse(seq, 0)).sum

    Set
      .from(
        for
          m  <- monkeys.iterator
          dx <- m.deltas.sliding(4)
        yield dx
      )
      .par
      .map(score)
      .max

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
