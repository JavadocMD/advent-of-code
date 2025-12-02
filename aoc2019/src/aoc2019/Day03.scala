package aoc2019

import scala.io.Source
import scala.annotation.tailrec
import fastparse._
import NoWhitespace._

import aoc.Day

object Day03 extends Day:

  sealed trait Direction
  case object Up    extends Direction
  case object Down  extends Direction
  case object Left  extends Direction
  case object Right extends Direction

  case class Run(direction: Direction, length: Int)
  case class WireRuns(a: List[Run], b: List[Run])
  type Input = WireRuns

  def parseNumber[$: P] = P(CharIn("0123456789").rep.!).map(_.toInt)
  def parseDirection[$: P] = P(CharIn("UDLR").!).map[Direction] {
    case "U" => Up
    case "D" => Down
    case "L" => Left
    case "R" => Right
  }
  def parseRun[$: P]  = P(parseDirection ~ parseNumber).map({ case (dir, num) => Run(dir, num) })
  def parseRuns[$: P] = P(Start ~ parseRun.rep(sep = ",") ~ End)

  def parse(xs: Array[String]): Input = xs.toList match
    case first :: second :: Nil =>
      val a = fastparse.parse(first, parseRuns(_))
      val b = fastparse.parse(second, parseRuns(_))
      if (!a.isSuccess || !b.isSuccess) throw new Exception("Could not parse input.")
      else WireRuns(a.get.value.toList, b.get.value.toList)
    case _ => throw new Exception("Could not parse input.")

  case class Point(x: Int, y: Int):
    def extend(direction: Direction, length: Int): Point = direction match {
      case Up    => Point(x, y + length)
      case Down  => Point(x, y - length)
      case Left  => Point(x - length, y)
      case Right => Point(x + length, y)
    }
    def manhattan(p: Point): Int = Math.abs(p.x - this.x) + Math.abs(p.y - this.y)

  object Point:
    val zero = Point(0, 0)

  case class Segment(start: Point, end: Point, cost: Int)

  object Segment:
    def from(runs: List[Run], initial: Point): List[Segment] =
      @tailrec
      def recurse(xs: List[Run], start: Point, cost: Int, acc: List[Segment]): List[Segment] = xs match {
        case Nil => acc
        case run :: tail =>
          val end = start.extend(run.direction, run.length)
          recurse(tail, end, cost + run.length, Segment(start, end, cost) :: acc)
      }
      recurse(runs, initial, 0, Nil).reverse

  def intersect(a: Segment, b: Segment): Option[Point] = {
    // Note: this assumes there can be at most one intersection which will fall at an integral coordinate.
    // Colinear segments will, for example, (seemingly always) return None.
    val s1 = Point(a.end.x - a.start.x, a.end.y - a.start.y)
    val s2 = Point(b.end.x - b.start.x, b.end.y - b.start.y)

    val s = (-s1.y * (a.start.x - b.start.x) + s1.x * (a.start.y - b.start.y)).toFloat / (-s2.x * s1.y + s1.x * s2.y)
    val t = (s2.x * (a.start.y - b.start.y) - s2.y * (a.start.x - b.start.x)).toFloat / (-s2.x * s1.y + s1.x * s2.y)

    if (s >= 0 && s <= 1 && t >= 0 && t <= 1)
      Some(
        Point(
          (a.start.x + (t * s1.x)).toInt,
          (a.start.y + (t * s1.y)).toInt
        )
      )
    else None
  }

  case class Intersection(point: Point, segmentA: Segment, segmentB: Segment)

  def intersectAll(segmentsA: List[Segment], segmentsB: List[Segment]): List[Intersection] =
    for {
      sa <- segmentsA
      sb <- segmentsB
      p  <- intersect(sa, sb)
    } yield Intersection(p, sa, sb)

  def bestIntersection(wires: WireRuns, cost: Intersection => Int): Option[Int] =
    val as = Segment.from(wires.a, Point.zero)
    val bs = Segment.from(wires.b, Point.zero)
    val is = intersectAll(as, bs).filter(i => i.point.x != 0 && i.point.y != 0)
    if (is.length == 0) None
    else Some(is.map(cost).min)

  def nearestIntersection(wires: WireRuns): Option[Int] =
    def cost(i: Intersection): Int = i.point.manhattan(Point.zero)
    bestIntersection(wires, cost)

  def shortestIntersection(wires: WireRuns): Option[Int] =
    def cost(i: Intersection): Int = {
      val a = i.segmentA.cost + i.segmentA.start.manhattan(i.point)
      val b = i.segmentB.cost + i.segmentB.start.manhattan(i.point)
      a + b
    }
    bestIntersection(wires, cost)

  def part1(input: Input): Long =
    nearestIntersection(input) match
      case Some(n) => n
      case None    => throw Exception("No intersection.")

  def part2(input: Input): Long =
    shortestIntersection(input) match
      case Some(n) => n
      case None    => throw Exception("No intersection.")

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
