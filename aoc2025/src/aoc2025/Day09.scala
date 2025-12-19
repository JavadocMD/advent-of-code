package aoc2025

import scala.annotation.tailrec
import aoc.Day
import aoc2025.Util.Vector2
import aoc2025.Util.Direction

object Day09 extends Day:

  type Input = List[Vector2]

  def parse(xs: List[String]): Input = xs.map:
    case s"$x,$y" => Vector2(x.toInt, y.toInt)

  case class Line(a: Vector2, b: Vector2):
    val xMin: Int = if (a.x < b.x) a.x else b.x
    val xMax: Int = if (a.x > b.x) a.x else b.x
    val yMin: Int = if (a.y < b.y) a.y else b.y
    val yMax: Int = if (a.y > b.y) a.y else b.y

    def overlaps(other: Line): Boolean =
      xMax >= other.xMin && other.xMax >= xMin &&
        yMax >= other.yMin && other.yMax >= yMin
  end Line

  // A rectangle is formed by defining two opposite corners.
  case class Rectangle(a: Vector2, b: Vector2):
    def area: Long = ((b.x - a.x).abs + 1).toLong * ((b.y - a.y).abs + 1)

    def sides: List[Line] =
      val c = Vector2(a.x, b.y)
      val d = Vector2(b.x, a.y)
      List(Line(a, c), Line(c, b), Line(b, d), Line(d, a))
  end Rectangle

  def pairs[T, U](xs: List[T], map: (T, T) => U): Iterator[U] =
    for
      ps <- xs.tails if ps.nonEmpty
      a = ps.head
      b <- ps.tail
    yield map(a, b)

  def part1(input: Input) = pairs(input, (a, b) => Rectangle(a, b).area).max

  def loopPairs[T](xs: List[T]): Iterator[(T, T)] =
    val a = LazyList.from(xs)
    val b = LazyList.from(xs.tail) :+ xs.head
    (a lazyZip b).iterator

  def loopTriplets[T](xs: List[T]): Iterator[(T, T, T)] =
    val a = LazyList.from(xs)
    val b = LazyList.from(xs.tail) :+ xs.head
    val c = LazyList.from(xs.tail.tail) :+ xs.head :+ xs.tail.head
    (a lazyZip b lazyZip c).iterator

  def direction(src: Vector2, dst: Vector2): Vector2 =
    if (src.x == dst.x) {
      if src.y < dst.y then Direction.N else Direction.S
    } else {
      if src.x < dst.x then Direction.E else Direction.W
    }

  sealed trait Turn
  case object CW  extends Turn
  case object CCW extends Turn

  def turns(input: Input): Iterator[(Vector2, Vector2, Turn)] =
    for (a, b, c) <- loopTriplets(input)
    yield
      val entering = direction(a, b)
      val leaving  = direction(b, c)
      val turn = (entering, leaving) match
        case (Direction.E, Direction.N) => CW
        case (Direction.E, Direction.S) => CCW
        case (Direction.W, Direction.N) => CCW
        case (Direction.W, Direction.S) => CW
        case (Direction.N, Direction.E) => CCW
        case (Direction.N, Direction.W) => CW
        case (Direction.S, Direction.E) => CW
        case (Direction.S, Direction.W) => CCW
        case _                          => throw new Exception("unexpected turn")
      (b, entering, turn)

  def outerCorner(adt: (Vector2, Vector2, Turn)): Vector2 =
    // Assumes clockwise traversal...
    val (point, enterDirection, turn) = adt
    (enterDirection, turn) match
      case (Direction.N, CW)  => Vector2(point.x + 1, point.y + 1)
      case (Direction.N, CCW) => Vector2(point.x + 1, point.y - 1)
      case (Direction.S, CW)  => Vector2(point.x - 1, point.y - 1)
      case (Direction.S, CCW) => Vector2(point.x - 1, point.y + 1)
      case (Direction.E, CW)  => Vector2(point.x + 1, point.y - 1)
      case (Direction.E, CCW) => Vector2(point.x - 1, point.y - 1)
      case (Direction.W, CW)  => Vector2(point.x - 1, point.y + 1)
      case (Direction.W, CCW) => Vector2(point.x + 1, point.y + 1)
      case _                  => throw new Exception("unexpected corner")

  def part2(input: Input) =
    val borderTurns = turns(input).toList

    // Verify clockwise traversal...
    // If number of CW turns is 4 greater, then the loop is CW.
    // Turns are 90 degrees and we must turn 360 degrees to return to the start.
    val isClockwise = 4 == (borderTurns.count(_._3 == CW) - borderTurns.count(_._3 == CCW))
    println(s"Traversal is clockwise? $isClockwise")

    val outBorderPoints = borderTurns.map(outerCorner).toList
    val outBorderLines = loopPairs(outBorderPoints).foldLeft(List.empty[Line]):
      case (acc, (a, b)) => Line(a, b) :: acc

    def isInBounds(line: Line): Boolean = !outBorderLines.exists(line.overlaps(_))

    pairs(input, (a, b) => Rectangle(a, b)).toList
      .sortBy(-_.area)
      .find(_.sides.forall(isInBounds))
      .get
      .area

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
