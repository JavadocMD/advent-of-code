package aoc2015

import scala.annotation.tailrec
import aoc.Day
import aoc2015.Util._

object Day20 extends Day:

  lazy val input = loadInput().toList.head.toInt

  def divisorSum(n: Int): Int =
    (1 to Math.sqrt(n).toInt).iterator.foldLeft(0):
      case (acc, curr) if n % curr != 0 => acc
      case (acc, curr) => acc + curr + (n / curr)

  lazy val part1 =
    val presentsPerHouse = Iterator.from(1).map(i => (i, divisorSum(i) * 10))
    presentsPerHouse.find(_._2 >= input).map(_._1).get

  def divisorSumWithMax(maxDivisors: Int)(n: Int): Int =
    (1 to Math.sqrt(n).toInt).iterator.foldLeft(0):
      case (acc, curr) if n % curr != 0 => acc
      case (acc, curr) =>
        val dual        = n / curr
        val includeCurr = n / curr <= maxDivisors
        val includeDual = n / dual <= maxDivisors
        (includeCurr, includeDual) match
          case (true, true)  => acc + curr + dual
          case (true, false) => acc + curr
          case (false, true) => acc + dual
          case _             => acc

  lazy val part2 =
    val divs             = divisorSumWithMax(50)
    val presentsPerHouse = Iterator.from(1).map(i => (i, divs(i) * 11))
    presentsPerHouse.find(_._2 >= input).map(_._1).get

  final def main(args: Array[String]): Unit =
    input
    solveP1(() => part1)
    solveP2(() => part2)
