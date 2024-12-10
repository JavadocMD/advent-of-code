package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day10 extends Day:

  case class Vector2(x: Int, y: Int):
    def +(other: Vector2): Vector2 = Vector2(this.x + other.x, this.y + other.y)

  object Direction:
    val E   = Vector2(1, 0)
    val S   = Vector2(0, 1)
    val W   = Vector2(-1, 0)
    val N   = Vector2(0, -1)
    val all = Seq(E, S, W, N)

  case class TopoMap(val tiles: Map[Vector2, Int], val starts: List[Vector2]):
    def neighborsUp1(of: Vector2): Seq[Vector2] =
      val currHeight = tiles(of)
      Direction.all
        .map(_ + of)
        .filter: p =>
          tiles.getOrElse(p, -9) == currHeight + 1

  type Input = TopoMap

  def parse(xs: List[String]): Input =
    val tiles = Map.from(
      for
        (row, y)    <- xs.zipWithIndex
        (height, x) <- row.zipWithIndex
      yield Vector2(x, y) -> height.toString.toInt
    )
    val starts = List.from(
      for
        (row, y)    <- xs.zipWithIndex
        (height, x) <- row.zipWithIndex
        if height.toString.toInt == 0
      yield Vector2(x, y)
    )
    TopoMap(tiles, starts)

  def findTrails(start: Vector2, map: TopoMap): Int =
    @tailrec
    def recurse(curr: Vector2, open: List[Vector2], acc: Set[Vector2]): Int =
      val nextAcc  = if map.tiles(curr) == 9 then acc + curr else acc
      val nextOpen = map.neighborsUp1(curr).toList ::: open
      nextOpen match
        case Nil          => nextAcc.size
        case head :: tail => recurse(head, tail, nextAcc)
    recurse(start, Nil, Set.empty)

  def part1(input: Input) = input.starts.map(findTrails(_, input)).sum

  def findTrails2(start: Vector2, map: TopoMap): Int =
    @tailrec
    def recurse(curr: Vector2, open: List[Vector2], acc: Int): Int =
      val nextAcc  = if map.tiles(curr) == 9 then acc + 1 else acc
      val nextOpen = map.neighborsUp1(curr).toList ::: open
      nextOpen match
        case Nil          => nextAcc
        case head :: tail => recurse(head, tail, nextAcc)
    recurse(start, Nil, 0)

  def part2(input: Input) = input.starts.map(findTrails2(_, input)).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
