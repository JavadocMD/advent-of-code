package aoc2017

import scala.annotation.tailrec
import aoc.Day
import scala.math.abs

object Day03 extends Day:

  lazy val input = loadInput().head.toInt

  def spiral(): Iterator[Vector2] =
    def line(start: Vector2, step: Vector2): Iterator[Vector2] =
      Iterator.iterate(start)(_ + step)

    def spiralFrom(start: Vector2, width: Int): Iterator[Vector2] =
      val wm2 = width - 2
      val a   = start
      val b   = a + Vector2(-1, +wm2)
      val c   = b + Vector2(-wm2, -1)
      val d   = c + Vector2(+1, -wm2)

      val as = line(a, Vector2(+0, +1)).take(width - 1)
      val bs = line(b, Vector2(-1, +0)).take(width - 1)
      val cs = line(c, Vector2(+0, -1)).take(width - 1)
      val ds = line(d, Vector2(+1, +0)).take(width - 1)
      as ++ bs ++ cs ++ ds

    val starts = line(Vector2(1, 0), Vector2(1, -1))
    val widths = Iterator.from(3, step = 2)
    (starts zip widths).flatMap:
      case (s, w) => spiralFrom(s, w)
  end spiral

  lazy val part1 =
    val p = spiral().drop(input - 2).next()
    Vector2.zero.manhattan(p)

  lazy val part2 =
    @tailrec
    def recurse(points: Iterator[Vector2], values: Map[Vector2, Int]): Int =
      val p    = points.next()
      val curr = p.neighbors8.map(values).sum
      if curr > input then curr
      else recurse(points, values.updated(p, curr))

    val initialValues = Map(Vector2(0, 0) -> 1).withDefaultValue(0)
    recurse(spiral(), initialValues)

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
