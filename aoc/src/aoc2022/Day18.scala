package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day18 extends Day:
  case class Vector3(x: Int, y: Int, z: Int):
    def neighbors = List(
      Vector3(x - 1, y, z),
      Vector3(x + 1, y, z),
      Vector3(x, y - 1, z),
      Vector3(x, y + 1, z),
      Vector3(x, y, z - 1),
      Vector3(x, y, z + 1)
    )

  type Input = List[Vector3]
  def parse(xs: Iterator[String]): Input = xs.map { case s"${IntP(x)},${IntP(y)},${IntP(z)}" =>
    Vector3(x, y, z)
  }.toList

  def surfaceArea(xs: Iterable[Vector3]): Int = xs
    .foldLeft((Set.empty[Vector3], 0)) { case ((cubes, area), x) =>
      val ns = x.neighbors.count(cubes.contains(_))
      (cubes + x, area + 6 - 2 * ns)
    }
    ._2

  def part1(input: Input) = surfaceArea(input)

  def extents(cubes: List[Vector3]): (Vector3, Vector3) =
    var min = Vector3(Int.MaxValue, Int.MaxValue, Int.MaxValue)
    var max = Vector3(Int.MinValue, Int.MinValue, Int.MinValue)
    for Vector3(x, y, z) <- cubes do
      if x < min.x then min = min.copy(x = x)
      if x > max.x then max = max.copy(x = x)
      if y < min.y then min = min.copy(y = y)
      if y > max.y then max = max.copy(y = y)
      if z < min.z then min = min.copy(z = z)
      if z > max.z then max = max.copy(z = z)
    (min, max)

  def raycast(cubes: Set[Vector3], ray: List[Vector3]): Set[Vector3] =
    @tailrec def recurse(ray: List[Vector3], pocket: Set[Vector3], acc: Set[Vector3]): Set[Vector3] = ray match
      case Nil                                  => acc
      case head :: tail if cubes.contains(head) => recurse(tail, Set.empty, pocket union acc)
      case head :: tail                         => recurse(tail, pocket + head, acc)
    recurse(ray.dropWhile(!cubes.contains(_)), Set.empty, Set.empty)

  def isInterior(cubes: Set[Vector3], min: Vector3, max: Vector3)(point: Vector3): Boolean =
    if cubes.contains(point) then false
    else
      (for x <- min.x until point.x yield Vector3(x, point.y, point.z)).exists(cubes.contains(_)) &&
      (for x <- point.x + 1 to max.x yield Vector3(x, point.y, point.z)).exists(cubes.contains(_)) &&
      (for y <- min.y until point.y yield Vector3(point.x, y, point.z)).exists(cubes.contains(_)) &&
      (for y <- point.y + 1 to max.y yield Vector3(point.x, y, point.z)).exists(cubes.contains(_)) &&
      (for z <- min.z until point.z yield Vector3(point.x, point.y, z)).exists(cubes.contains(_)) &&
      (for z <- point.z + 1 to max.z yield Vector3(point.x, point.y, z)).exists(cubes.contains(_))

  def floodExterior(cubes: Set[Vector3], min: Vector3, max: Vector3): Set[Vector3] =
    def recurse(frontier: List[Vector3], acc: Set[Vector3]): Set[Vector3] =
      frontier match
        case Nil => acc
        case head :: tail =>
          val ns = for
            v <- head.neighbors
            if !acc.contains(v) && !cubes.contains(v)
            if v.x >= min.x && v.y >= min.y && v.z >= min.z
            if v.x <= max.x && v.y <= max.y && v.z <= max.z
          yield v
          recurse(ns ::: tail, acc ++ ns)
    recurse(min :: Nil, Set.empty)

  def part2(input: Input) =
    val cubes      = input.toSet
    val (min, max) = extents(input)
    val minp       = Vector3(min.x - 1, min.y - 1, min.z - 1)
    val maxp       = Vector3(max.x + 1, max.y + 1, max.z + 1)

    val notCubes = (for {
      x <- minp.x to maxp.x
      y <- minp.y to maxp.y
      z <- minp.z to maxp.z
      v = Vector3(x, y, z)
      if !cubes.contains(v)
    } yield v).toSet

    val exterior = floodExterior(cubes, minp, maxp)
    val interior = notCubes -- exterior

    surfaceArea(cubes) - surfaceArea(interior)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
