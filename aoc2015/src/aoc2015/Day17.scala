package aoc2015

import scala.annotation.tailrec
import aoc.Day

object Day17 extends Day:

  type Input = List[Int]

  def parse(xs: List[String]): Input = xs.map(_.toInt)

  lazy val input = parse(loadInput().toList)

  lazy val part1 =
    val combos = for
      n    <- (1 to input.size).iterator
      bins <- input.zipWithIndex.combinations(n)
      if bins.map(_._1).sum == 150
    yield 1
    combos.sum

  lazy val part2 =
    val combos = for
      n    <- (1 to input.size).iterator
      bins <- input.zipWithIndex.combinations(n)
      if bins.map(_._1).sum == 150
    yield (bins.size, 1)

    val combosByBinsCount = combos.foldLeft(Map.empty[Int, Int].withDefaultValue(0)):
      case (acc, (size, n)) => acc.updated(size, acc(size) + n)
    combosByBinsCount.minBy(_._1)._2

  final def main(args: Array[String]): Unit =
    solveP1(() => part1)
    solveP2(() => part2)
