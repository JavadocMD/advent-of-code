package aoc2021

import scala.annotation.tailrec

import aoc.Day

object Day09 extends Day:
  case class Point(x: Int, y: Int)
  case class Input(grid: Map[Point, Int], width: Int, height: Int)

  def parse(xs: Array[String]): Input =
    val tiles = for
      (line, y) <- xs.zipWithIndex
      (c, x)    <- line.zipWithIndex
    yield Point(x, y) -> c.asDigit
    Input(Map.from(tiles), xs(0).size, xs.size)

  def adjacent(p: Point)(using in: Input): List[Point] =
    val Point(x, y) = p
    var ps          = List.empty[Point]
    if (y > 0) ps ::= Point(x, y - 1)
    if (x + 1 < in.width) ps ::= Point(x + 1, y)
    if (y + 1 < in.height) ps ::= Point(x, y + 1)
    if (x > 0) ps ::= Point(x - 1, y)
    ps

  def isLow(p: Point)(using in: Input): Boolean =
    val h = in.grid(p)
    adjacent(p).forall(q => in.grid(q) > h)

  def part1(input: Input) =
    given Input = input
    input.grid
      .filter({ case (p, _) => isLow(p) })
      .map({ case (_, h) => h + 1 })
      .sum

  @tailrec
  def findBasin(frontier: Set[Point], basin: Set[Point] = Set.empty)(using in: Input): Set[Point] =
    if frontier.isEmpty then basin
    else
      val adj = for
        p <- frontier
        q <- adjacent(p)
        if in.grid(q) < 9 && !basin.contains(q)
      yield q
      findBasin(adj, basin ++ adj)

  def part2(input: Input) =
    given Input = input
    val basins =
      for low <- input.grid.keys.filter(isLow).toList
      yield findBasin(Set(low))
    basins.map(_.size).sorted.takeRight(3).product

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
