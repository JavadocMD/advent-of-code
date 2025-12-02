package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day06 extends Day:

  // Time:        47     70     75     66
  // Distance:   282   1079   1147   1062

  case class Race(length: Long, record: Long):
    def distance(hold: Long): Long =
      val speed = hold
      val time  = length - hold
      speed * time

  type Input = Unit

  def part1(input: Input) =
    val races = List(
      Race(47, 282),
      Race(70, 1079),
      Race(75, 1147),
      Race(66, 1062),
    )

    def waysToWin(race: Race): Int =
      (0 until race.length.toInt)
        .map(x => race.distance(x.toLong))
        .count(_ > race.record)

    races.map(waysToWin).product

  def part2(input: Input) =
    val race = Race(47707566L, 282107911471062L)

    @tailrec
    def search(hold: Long, step: Long, wins: Long = 0): Long =
      val d = race.distance(hold)
      if d <= race.record then wins
      else search(hold + step, step, wins + 1)

    search(1 + race.length / 2, 1) + search(race.length / 2, -1)

  final def main(args: Array[String]): Unit =
    val in = {}
    solveP1(() => part1(in))
    solveP2(() => part2(in))
