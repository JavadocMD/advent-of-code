package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day08 extends Day:

  case class Vector2(x: Int, y: Int):
    def +(other: Vector2): Vector2 = Vector2(this.x + other.x, this.y + other.y)
    def -(other: Vector2): Vector2 = Vector2(this.x - other.x, this.y - other.y)

  case class MapInfo(antennas: Map[Char, List[Vector2]], size: Vector2):
    def contains(p: Vector2): Boolean = p.x >= 0 && p.y >= 0 && p.x < size.x && p.y < size.y

  type Input = MapInfo

  def parse(lines: List[String]): Input =
    val allAntennas = for
      (ln, y) <- lines.zipWithIndex
      (ch, x) <- ln.zipWithIndex
      if ch != '.'
    yield (ch, Vector2(x, y))
    MapInfo(
      antennas = allAntennas.groupMap(_._1)(_._2),
      size = Vector2(lines.head.size, lines.size),
    )

  def antinodes(map: MapInfo): Set[Vector2] =
    val antinodes = for
      (c, positions) <- map.antennas
      case a :: b :: Nil <- positions.combinations(2)
      an <- Seq(a - (b - a), b + (b - a))
      if map.contains(an)
    yield an
    antinodes.toSet

  def part1(input: Input) = antinodes(input).size

  def extend(a: Vector2, b: Vector2, map: MapInfo): Iterable[Vector2] =
    val diff = b - a
    LazyList.iterate(b)(_ + diff).takeWhile(map.contains)

  def antinodes2(map: MapInfo): Set[Vector2] =
    val antinodes = for
      (c, positions) <- map.antennas
      case a :: b :: Nil <- positions.combinations(2)
      an <- extend(a, b, map) ++ extend(b, a, map)
    yield an
    antinodes.toSet

  def part2(input: Input) = antinodes2(input).size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
