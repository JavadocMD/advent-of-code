package aoc2015

import scala.annotation.tailrec
import aoc.Day
import scala.math.{min, floor}

object Day14 extends Day:

  case class Reindeer(name: String, speed: Int, duration: Int, rest: Int):
    val burstDistance = speed * duration
    val period        = duration + rest

    def distance(time: Int): Int =
      min(time % period, duration) * speed // flying distance, until duration is reached
        + floor(time / period).toInt * burstDistance // add complete periods

  type Input = List[Reindeer]

  def parse(xs: List[String]): Input = xs.map:
    case s"$name can fly $speed km/s for $duration seconds, but then must rest for $rest seconds." =>
      Reindeer(name, speed.toInt, duration.toInt, rest.toInt)

  lazy val input = parse(loadInput().toList)

  lazy val part1 = input.map(_.distance(2503)).max

  lazy val part2 =
    (1 to 2503)
      .foldLeft(Vector.fill(input.size)(0)):
        case (scores, time) =>
          val distances = input.map(_.distance(time))
          val best      = distances.max
          (scores zip distances).map:
            case (s, d) if d == best => s + 1
            case (s, _)              => s
      .max

  final def main(args: Array[String]): Unit =
    solveP1(() => part1)
    solveP2(() => part2)
