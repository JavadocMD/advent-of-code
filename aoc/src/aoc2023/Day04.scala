package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day04 extends Day:

  case class Card(id: Int, winners: Set[Int], numbers: List[Int]):
    def score: Int =
      val n = this.matches
      if n == 0 then 0 else math.pow(2, n - 1).toInt

    def matches: Int = numbers.count(winners.contains(_))

  type Input = List[Card]

  def parse(xs: List[String]): Input =
    for line <- xs yield line match
      case s"Card $id: $win | $num" =>
        val winners = win.split(" ").toSet.filter(_.nonEmpty).map(_.toInt)
        val numbers = num.split(" ").toList.filter(_.nonEmpty).map(_.toInt)
        Card(id.strip.toInt, winners, numbers)

  def part1(input: Input) =
    input.map(_.score).sum

  @tailrec
  def recurse(cards: List[Card], completed: List[(Long, Card)] = Nil, total: Long = 0): Long =
    cards match
      case Nil => total
      case head :: tail =>
        val m     = head.matches
        val value = completed.take(m).map(_._1).sum + 1
        recurse(tail, (value, head) :: completed, total + value)

  def part2(input: Input) =
    recurse(input.reverse)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
