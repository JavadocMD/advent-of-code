package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day17 extends Day:
  import scala.collection.{mutable => m}

  case class Vector3(x: Int, y: Int, z: Int)

  type Input = Set[Vector3]
  def parse(input: Array[String]): Input =
    Set.from(for {
      (row, y) <- input.zipWithIndex
      (c, x)   <- row.zipWithIndex if c == '#'
    } yield Vector3(x, y, 0))

  def evolve3d(active: Set[Vector3]): Set[Vector3] =
    val neighbors = m.Map.empty[Vector3, Int].withDefault(_ => 0)
    active.foreach { v =>
      for {
        z <- v.z - 1 to v.z + 1
        y <- v.y - 1 to v.y + 1
        x <- v.x - 1 to v.x + 1
        curr = Vector3(x, y, z)
        if curr != v
      } {
        neighbors(curr) += 1
      }
    }

    def nowActive(entry: (Vector3, Int)): Boolean = {
      val (v, n) = entry
      n == 3 || (n == 2 && active.contains(v))
    }

    neighbors.filter(nowActive).keys.toSet

  def part1(input: Input): Int =
    var active = input
    for { i <- 0 until 6 } {
      active = evolve3d(active)
    }
    active.size

  case class Vector4(x: Int, y: Int, z: Int, w: Int)

  def to4d(v: Vector3) = Vector4(v.x, v.y, v.z, 0)

  def evolve4d(active: Set[Vector4]): Set[Vector4] =
    val neighbors = m.Map.empty[Vector4, Int].withDefault(_ => 0)
    active.foreach { v =>
      for {
        w <- v.w - 1 to v.w + 1
        z <- v.z - 1 to v.z + 1
        y <- v.y - 1 to v.y + 1
        x <- v.x - 1 to v.x + 1
        curr = Vector4(x, y, z, w)
        if curr != v
      } {
        neighbors(curr) += 1
      }
    }

    def nowActive(entry: (Vector4, Int)): Boolean = {
      val (v, n) = entry
      n == 3 || (n == 2 && active.contains(v))
    }

    neighbors.filter(nowActive).keys.toSet

  def part2(input: Input): Int =
    var active = input.map(to4d)
    for { i <- 0 until 6 } {
      active = evolve4d(active)
    }
    active.size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
