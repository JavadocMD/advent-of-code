package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day18 extends Day:

  case class Instruction(dir: Dir, steps: Int, color: String)

  type Input = List[Instruction]

  def parse(xs: List[String]): Input = xs
    .map:
      case s"L $steps (#$color)" => (Left, steps.toInt, color)
      case s"R $steps (#$color)" => (Right, steps.toInt, color)
      case s"U $steps (#$color)" => (Up, steps.toInt, color)
      case s"D $steps (#$color)" => (Down, steps.toInt, color)
    .map(Instruction.apply)

  def decodeColor(x: Instruction): Instruction =
    x.color.splitAt(5) match
      case (d, "0") => Instruction(Right, Integer.parseInt(d, 16), x.color)
      case (d, "1") => Instruction(Down, Integer.parseInt(d, 16), x.color)
      case (d, "2") => Instruction(Left, Integer.parseInt(d, 16), x.color)
      case (d, "3") => Instruction(Up, Integer.parseInt(d, 16), x.color)

  // I think this corner logic assumes we're traveling the loop clockwise
  def findCorner(cell: Point, enters: Dir, leaves: Dir): Point =
    (enters, leaves) match
      case (Right, Up)   => Point(cell.x, cell.y)
      case (Right, Down) => Point(cell.x + 1, cell.y)
      case (Left, Up)    => Point(cell.x, cell.y + 1)
      case (Left, Down)  => Point(cell.x + 1, cell.y + 1)
      case (Up, Left)    => Point(cell.x, cell.y + 1)
      case (Up, Right)   => Point(cell.x, cell.y)
      case (Down, Left)  => Point(cell.x + 1, cell.y + 1)
      case (Down, Right) => Point(cell.x + 1, cell.y)
      case x             => throw Exception(s"Unhandled case! $x")

  def dig2(xs: List[Instruction]): List[Point] =
    // This calculates the points along the perimeter of the dug area,
    // so the returned points are in vertex space, not cell coordinate space.
    val result = xs
      .zip(xs.tail)
      .foldLeft((List.empty[Point], Point.zero)):
        case ((points, prevPos), (curr, next)) =>
          val currPos = prevPos.goN(curr.dir, curr.steps)
          val p       = findCorner(currPos, curr.dir, next.dir)
          (p :: points, currPos)

    // have to prepend and append the first cell
    // (so we can iterate over pairs later)
    val p0 = findCorner(Point.zero, xs.last.dir, xs.head.dir)
    p0 :: (p0 :: result._1).reverse

  def polyArea(points: List[Point]): Long =
    val areaX2 = points
      .zip(points.tail)
      .foldLeft(0L) { case (acc, (a, b)) =>
        acc + (a.x.toLong * b.y.toLong - b.x.toLong * a.y.toLong)
      }
    areaX2 / 2L

  def part1(input: Input) =
    polyArea(dig2(input))

  def part2(input: Input) =
    polyArea(dig2(input.map(decodeColor)))

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
