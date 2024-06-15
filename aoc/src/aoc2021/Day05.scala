package aoc2021

import scala.annotation.tailrec

import aoc.Day

object Day05 extends Day:
  type Input = List[Line]

  def lineRange(a: Int, b: Int) =
    if (a <= b) then a to b else a to b by -1

  case class Point(x: Int, y: Int)

  case class Line(x1: Int, y1: Int, x2: Int, y2: Int):
    def points: Seq[Point] =
      if (x1 == x2) for y <- lineRange(y1, y2) yield Point(x1, y)
      else if (y1 == y2) for x <- lineRange(x1, x2) yield Point(x, y1)
      else
        for (x, y) <- lineRange(x1, x2).zip(lineRange(y1, y2))
        yield Point(x, y)

  def parse(input: Array[String]): Input =
    input.toList.map(s => {
      val pairs = s.split(" -> ").map(_.split(",").map(_.toInt))
      Line(pairs(0)(0), pairs(0)(1), pairs(1)(0), pairs(1)(1))
    })

  def part1(input: Input) =
    val z = Map.empty[Point, Int]
    val grid = input
      .filter(a => a.x1 == a.x2 || a.y1 == a.y2)
      .flatMap(_.points)
      .foldLeft(z) { case (map, p) =>
        val count = map.getOrElse(p, 0)
        map.updated(p, count + 1)
      }
    grid.count(_._2 > 1)

  def part2(input: Input) =
    val z = Map.empty[Point, Int]
    val grid = input
      .flatMap(_.points)
      .foldLeft(z) { case (map, p) =>
        val count = map.getOrElse(p, 0)
        map.updated(p, count + 1)
      }
    grid.count(_._2 > 1)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
