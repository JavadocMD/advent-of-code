package aoc2021

import scala.annotation.tailrec
import Math.{min, max}
import scala.collection.immutable.SortedSet
import scala.collection.{mutable => m}
import scala.collection.View

import aoc.Day

object Day22 extends Day:
  // Our ranges are non-inclusive, but the input's are inclusive.
  case class Step(on: Boolean, x0: Int, x1: Int, y0: Int, y1: Int, z0: Int, z1: Int):
    def volume: Long = (x1 - x0).toLong * (y1 - y0).toLong * (z1 - z0).toLong
    def contains(x: Int, y: Int, z: Int): Boolean =
      x0 <= x && x < x1 &&
        y0 <= y && y < y1 &&
        z0 <= z && z < z1

  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  case class Vector(x: Int, y: Int, z: Int)

  def part1(input: Input) =
    val steps = for line <- input.toList yield line match
      case s"$onoff x=$x0..$x1,y=$y0..$y1,z=$z0..$z1" =>
        Step(
          onoff == "on",
          max(-50, x0.toInt),
          min(51, x1.toInt + 1),
          max(-50, y0.toInt),
          min(51, y1.toInt + 1),
          max(-50, z0.toInt),
          min(51, z1.toInt + 1)
        )
    val cubesOn = steps.foldLeft(Set.empty[Vector]) { (cubesOn, curr) =>
      var xs = cubesOn
      for
        x <- curr.x0 until curr.x1
        y <- curr.y0 until curr.y1
        z <- curr.z0 until curr.z1
      do
        if curr.on then xs += Vector(x, y, z)
        else xs -= Vector(x, y, z)
      xs
    }
    cubesOn.size

  def points(steps: List[Step]): (View[Int], View[Int], View[Int]) =
    val Step(_, x0, x1, y0, y1, z0, z1) = steps.head

    var xs = m.SortedSet.empty[Int]
    var ys = m.SortedSet.empty[Int]
    var zs = m.SortedSet.empty[Int]
    steps.foreach(s =>
      if (x0 <= s.x0 && s.x0 < x1) xs += s.x0
      if (x0 <= s.x1 && s.x1 < x1) xs += s.x1
      if (y0 <= s.y0 && s.y0 < y1) ys += s.y0
      if (y0 <= s.y1 && s.y1 < y1) ys += s.y1
      if (z0 <= s.z0 && s.z0 < z1) zs += s.z0
      if (z0 <= s.z1 && s.z1 < z1) zs += s.z1
    )
    xs += x1
    ys += y1
    zs += z1
    (xs.view, ys.view, zs.view)

  @tailrec
  def cubesOn(steps: List[Step], acc: Long): Long = steps match
    case Nil => acc
    // if this step isn't "on", it contributes nothing to the on count
    case head :: tail if !head.on => cubesOn(tail, acc)
    // if this step is fully enclosed in another step, it contributes no cubes
    // case head :: tail if tail.find(s => inside(head, s)).isDefined => cubesOn(tail, acc)
    case head :: tail =>
      def isMine(x: Int, y: Int, z: Int): Boolean =
        tail.find(s => s.contains(x, y, z)).isEmpty
      val (xs, ys, zs) = points(steps)
      var count        = head.volume
      for
        (x, nx) <- xs.zip(xs.tail)
        (y, ny) <- ys.zip(ys.tail)
        (z, nz) <- zs.zip(zs.tail)
        if !isMine(x, y, z)
      do count -= (nx - x).toLong * (ny - y).toLong * (nz - z).toLong
      cubesOn(tail, acc + count)

  def part2(input: Input) =
    val steps = (for line <- input.toList yield line match
      case s"$onoff x=$x0..$x1,y=$y0..$y1,z=$z0..$z1" =>
        Step(
          onoff == "on",
          x0.toInt,
          x1.toInt + 1,
          y0.toInt,
          y1.toInt + 1,
          z0.toInt,
          z1.toInt + 1
        )
    )
    cubesOn(steps, 0L)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
