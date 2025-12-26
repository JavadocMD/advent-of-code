package aoc2015

import scala.annotation.tailrec
import aoc.Day
import aoc2015.Util._

object Day25 extends Day:

  type Input = (Int, Int)

  def parse(xs: List[String]): Input =
    val nums = xs.head
      .split("""[\s.,]+""")
      .toList
      .flatMap: s =>
        s.toIntOption
    (nums.head, nums.tail.head)

  lazy val input = parse(loadInput().toList)

  def iteration(row: Int, col: Int): Int =
    val i = Iterator
      .from(1)
      .scanLeft(1):
        case (acc, n) => acc + n
      .drop(row - 1)
      .next
    val j = Iterator
      .from(row + 1)
      .scanLeft(i):
        case (acc, n) => acc + n
      .drop(col - 1)
      .next
    j

  lazy val codes: LazyList[Long] = 20151125L #:: codes.map:
    case n => (n * 252533L) % 33554393L

  lazy val part1 =
    val (row, col) = input
    codes.drop(iteration(row, col) - 1).head

  lazy val part2 = "MERRY CHRISTMAS"

  final def main(args: Array[String]): Unit =
    input
    solveP1(() => part1)
    solveP2(() => part2)
