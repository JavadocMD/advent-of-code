package aoc2015

import scala.annotation.tailrec
import aoc.Day
import aoc2015.Util.loopTriplets

object Day13 extends Day:

  type Input = Map[(String, String), Int]

  def parse(xs: List[String]): Input =
    xs.map:
      case s"$who would gain $x happiness units by sitting next to $nextTo." => (who, nextTo) -> x.toInt
      case s"$who would lose $x happiness units by sitting next to $nextTo." => (who, nextTo) -> -x.toInt
    .toMap

  lazy val input = parse(loadInput().toList)

  def score(scoring: Map[(String, String), Int])(order: List[String]): Int =
    loopTriplets(order)
      .map:
        case (a, b, c) => scoring((b, a)) + scoring((b, c))
      .sum

  lazy val part1 =
    val names  = input.keys.map(_._1).toSet.toList
    val scores = for order <- names.permutations yield score(input)(order)
    scores.max

  lazy val part2 =
    val origNames = input.keys.map(_._1).toSet.toList
    val names     = "you" :: origNames
    val scoring = input ++ origNames.flatMap: n =>
      ("you", n) -> 0 :: (n, "you") -> 0 :: Nil

    val scores = for order <- names.permutations yield score(scoring)(order)
    scores.max

  final def main(args: Array[String]): Unit =
    solveP1(() => part1)
    solveP2(() => part2)
