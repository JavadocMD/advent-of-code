package aoc2015

import scala.annotation.tailrec
import aoc.Day

object Day09 extends Day:

  case class Leg(a: String, b: String, distance: Int)

  type Input = List[Leg]

  def parse(xs: List[String]): Input = xs.map:
    case s"$a to $b = $d" => Leg(a, b, d.toInt)

  def allRouteDistances(legs: List[Leg]): Iterator[Int] =
    val locations = legs.view.flatMap(r => r.a :: r.b :: Nil).toSet
    val distances = legs.flatMap(r => (r.a, r.b) -> r.distance :: (r.b, r.a) -> r.distance :: Nil).toMap

    @tailrec
    def routeDistance(stops: List[String], acc: Int = 0): Option[Int] = stops match
      case Nil      => Some(acc)
      case _ :: Nil => Some(acc)
      case a :: (tail @ (b :: _)) =>
        distances.get((a, b)) match
          case Some(d) => routeDistance(tail, acc + d)
          case None    => None // there may be no path from a -> b

    for
      start <- locations.iterator
      rest  <- locations.excl(start).toList.permutations
      dist  <- routeDistance(start :: rest)
    yield dist

  def part1(input: Input) = allRouteDistances(input).min

  def part2(input: Input) = allRouteDistances(input).max

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
