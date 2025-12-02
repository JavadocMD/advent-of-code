package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day02 extends Day:

  case class Set(green: Int, blue: Int, red: Int):
    def <=(other: Set): Boolean =
      this.green <= other.green && this.blue <= other.blue && this.red <= other.red

    def power: Long =
      green * blue * red

  case class Game(id: Int, sets: List[Set]):
    def max =
      sets.reduce { case (a, b) =>
        Set(
          a.green.max(b.green),
          a.blue.max(b.blue),
          a.red.max(b.red),
        )
      }

  type Input = List[Game]
  def parse(xs: Array[String]): Input =
    for line <- xs.toList yield line match
      case s"Game ${id}: $rest" =>
        val sets = for round <- rest.split("; ").toList yield
          var green = 0
          var blue  = 0
          var red   = 0
          for cube <- round.split(", ")
          yield cube match
            case s"$num green" => green = num.toInt
            case s"$num blue"  => blue = num.toInt
            case s"$num red"   => red = num.toInt
          Set(green, blue, red)
        Game(id.toInt, sets)
      case _ =>
        throw Exception(s"Can't parse line: $line")

  def part1(input: Input) =
    // only 12 red cubes, 13 green cubes, and 14 blue cubes
    val possible = Set(13, 14, 12)
    input
      .filter(game => game.max <= possible)
      .map(game => game.id)
      .sum

  def part2(input: Input) =
    input.map(game => game.max.power).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
