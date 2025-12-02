package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day09 extends Day:
  type Input = List[String]
  def parse(xs: Iterator[String]): Input = xs.toList

  case class Vector2(x: Int, y: Int):
    def minus(other: Vector2) = Vector2(x - other.x, y - other.y)
    def plus(other: Vector2)  = Vector2(x + other.x, y + other.y)

  object Vector2:
    val zero  = Vector2(0, 0)
    val up    = Vector2(0, 1)
    val down  = Vector2(0, -1)
    val left  = Vector2(-1, 0)
    val right = Vector2(1, 0)

  def stringify(width: Int, height: Int, visited: Set[Vector2]) =
    val grid = Vector.tabulate(width, height) { (y, x) =>
      if visited.contains(Vector2(x, y)) then '#' else '.'
    }
    grid.reverse.map(_.mkString).mkString("\n")

  def parseInstruction(s: String): (Vector2, Int) = s match
    case s"U $n" => (Vector2.up, n.toInt)
    case s"D $n" => (Vector2.down, n.toInt)
    case s"L $n" => (Vector2.left, n.toInt)
    case s"R $n" => (Vector2.right, n.toInt)

  def updateTail(tail: Vector2, head: Vector2): Vector2 =
    val Vector2(dx, dy) = head.minus(tail)
    if dx.abs > 1 || dy.abs > 1 then tail.plus(Vector2(dx.sign, dy.sign))
    else tail

  case class State(knots: List[Vector2], tailVisited: Set[Vector2])

  def simulateRope(instructions: List[String], ropeSize: Int): State =
    val z = State(List.fill(ropeSize)(Vector2.zero), Set(Vector2.zero))
    instructions.foldLeft(z) { (prevState, inst) =>
      val (dir, times) = parseInstruction(inst)

      var State(knots, visited) = prevState
      for i <- 0 until times do
        knots = knots.tail.scanLeft(knots.head.plus(dir)) { (prev, curr) =>
          updateTail(curr, prev)
        }
        visited = visited + knots.last

      State(knots, visited)
    }

  def part1(input: Input) = simulateRope(input, 2).tailVisited.size

  def part2(input: Input) = simulateRope(input, 10).tailVisited.size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
