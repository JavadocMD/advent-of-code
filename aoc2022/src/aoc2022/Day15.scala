package aoc2022

import scala.annotation.tailrec
import scala.math.{min, max}

import aoc.Day

object Day15 extends Day:
  import Grid2D.Vector2

  case class Reading(sensor: Vector2, beacon: Vector2):
    val distance = beacon.minus(sensor).manhattanLength

  type Input = List[Reading]
  def parse(xs: Iterator[String]): Input = xs.map {
    case s"Sensor at x=${IntP(xs)}, y=${IntP(ys)}: closest beacon is at x=${IntP(xb)}, y=${IntP(yb)}" =>
      Reading(Vector2(xs, ys), Vector2(xb, yb))
  }.toList

  case class Range(start: Int, end: Int):
    def size: Int                      = end - start + 1
    def iterator: Iterator[Int]        = Iterator.range(start, end + 1)
    def contains(x: Int): Boolean      = start <= x && x <= end
    def overlaps(that: Range): Boolean = this.start <= that.end && that.start <= this.end
    // only safe if we know `this` and `that` overlap!
    def union(that: Range) = Range(min(this.start, that.start), max(this.end, that.end))

  def projectReading(r: Reading, y: Int, minX: Int, maxX: Int): Option[Range] =
    val deltaY = (r.sensor.y - y).abs
    Option.when(deltaY <= r.distance) {
      val w = r.distance - deltaY
      val a = max(minX, r.sensor.x - w)
      val b = min(maxX, r.sensor.x + w)
      Range(a, b)
    }

  @tailrec
  def unionAll(xs: List[Range], acc: List[Range] = Nil): List[Range] =
    if xs.isEmpty then acc
    else
      xs.tail.partition(_.overlaps(xs.head)) match
        case (Nil, rest) => unionAll(rest, xs.head :: acc)
        case (ovs, rest) => unionAll(ovs.foldLeft(xs.head)(_ union _) :: rest, acc)

  def unifyProjections(
      readings: List[Reading],
      y: Int,
      minX: Int = Int.MinValue,
      maxX: Int = Int.MaxValue
  ): List[Range] =
    unionAll(readings.flatMap(projectReading(_, y, minX, maxX)))

  def part1(input: Input) =
    val row       = 2000000
    val beacons   = input.map(_.beacon).toSet
    val projected = unifyProjections(input, row)
    assert(projected.size == 1)
    val coverage = projected.head
    coverage.size - beacons.count(b => b.y == row && coverage.contains(b.x))
  end part1

  def part2(input: Input) =
    val mapRange = Range(0, 4000000)
    mapRange.iterator
      .map { y => (unifyProjections(input, y, mapRange.start, mapRange.end), y) }
      // the beacon hiding spot is in the first row with disjoint ranges
      .find { (rs, _) => rs.size == 2 }
      .map { (rs, y) =>
        // the x coordinate is after the left-most range
        val x = rs.minBy(_.end).end + 1
        println(s"($x,$y)")
        // calc tuning frequency
        x.toLong * 4000000L + y.toLong
      }
  end part2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in).get)
