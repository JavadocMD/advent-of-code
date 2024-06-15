package aoc2021

import scala.annotation.tailrec
import scala.collection.{mutable => m}
import scala.collection.View

import aoc.Day

object Day25 extends Day:
  enum Cucumber:
    case East
    case South
  import Cucumber._

  case class Seafloor(grid: Map[Vector, Cucumber], size: Vector):
    def tick: (Seafloor, Int) =
      var updates  = 0
      var eastTick = grid
      for (v, c) <- grid if c == East do
        val adj = Vector((v.x + 1) % size.x, v.y)
        if (!grid.contains(adj))
          updates += 1
          eastTick = eastTick.removed(v).updated(adj, c)
      var southTick = eastTick
      for (v, c) <- grid if c == South do
        val adj = Vector(v.x, (v.y + 1) % size.y)
        if (!eastTick.contains(adj))
          updates += 1
          southTick = southTick.removed(v).updated(adj, c)
      (this.copy(grid = southTick), updates)

  type Input = Seafloor
  def parse(xs: Array[String]): Input =
    val grid = m.Map.empty[Vector, Cucumber]
    for
      (line, y) <- xs.zipWithIndex
      (c, x)    <- line.zipWithIndex
    do
      c match
        case '>' => grid += (Vector(x, y) -> East)
        case 'v' => grid += (Vector(x, y) -> South)
        case _   => // ignore
    println(grid.size)
    println(Vector(xs.head.size, xs.size))
    Seafloor(grid.toMap, Vector(xs.head.size, xs.size))

  def part1(input: Input) =
    @tailrec
    def recurse(s: Seafloor, step: Int): Int =
      val (next, updates) = s.tick
      if (step % 1000 == 0)
        println(s"$step, $updates")
      if updates == 0 then step
      else recurse(next, step + 1)
    recurse(input, 1)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
