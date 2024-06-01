package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day22 extends Day:

  case class Coord(x: Int, y: Int, z: Int)

  case class Brick(label: Int, c0: Coord, c1: Coord):
    val low: Int  = math.min(c0.z, c1.z)
    val high: Int = math.max(c0.z, c1.z)
    val xy: List[(Int, Int)] = (c0, c1) match
      case (c0, c1) if c0.x != c1.x =>
        (math.min(c0.x, c1.x) to math.max(c0.x, c1.x)).map((_, c0.y)).toList
      case (c0, c1) if c0.y != c1.y =>
        (math.min(c0.y, c1.y) to math.max(c0.y, c1.y)).map((c0.x, _)).toList
      case (c0, c1) =>
        // vertical or single cube!
        List((c0.x, c0.y))

  type Input = List[Brick]

  def parse(xs: List[String]): Input =
    xs.zipWithIndex.map:
      case (s"$x0,$y0,$z0~$x1,$y1,$z1", label) =>
        val c0 = Coord(x0.toInt, y0.toInt, z0.toInt)
        val c1 = Coord(x1.toInt, y1.toInt, z1.toInt)
        Brick(label, c0, c1)

  case class Supported(brick: Brick, by: Set[Brick])

  def fall(bricks: List[Brick]): List[Supported] =
    // `highest` is: (x,y) -> (z, brick)
    @tailrec
    def recurse(remaining: List[Brick], highest: Map[(Int, Int), (Int, Brick)], acc: List[Supported]): List[Supported] =
      remaining match
        case Nil          => acc
        case head :: tail =>
          // These bricks are under head
          val under = head.xy.flatMap(p => highest.get(p))
          // This is the high z under head
          val high = under.maxByOption(_._1).map(_._1).getOrElse(0)
          // These bricks actually support head (they're at the high z)
          val supporting = under.filter(_._1 == high).map(_._2).toSet
          // Now we can drop the brick to its resting spot
          val zDelta = head.low - (high + 1)
          val c0     = head.c0
          val c1     = head.c1
          val atRest = Brick(head.label, Coord(c0.x, c0.y, c0.z - zDelta), Coord(c1.x, c1.y, c1.z - zDelta))
          // And update our tracking of which z is highest for each (x,y)
          val nextHighest = highest ++ atRest.xy.map({ case (x, y) => (x, y) -> (atRest.high, atRest) })
          recurse(tail, nextHighest, Supported(atRest, supporting) :: acc)

    recurse(bricks.sortBy(_.low), Map.empty, List.empty)

  def freeBricks(resting: List[Supported]): Set[Brick] =
    var free = resting.map(_._1).toSet
    // Find bricks that are supported by only one brick, then mark that brick not free.
    for x <- resting if x.by.size == 1
    do free -= x.by.head
    free

  def part1(input: Input) =
    freeBricks(fall(input)).size

  def cascadeTree(resting: List[Supported]): Map[Brick, Set[Brick]] =
    var tree = Map.empty[Brick, Set[Brick]]
    for
      supported  <- resting
      supporting <- supported.by
    do
      val set = tree.getOrElse(supporting, Set.empty)
      tree = tree.updated(supporting, set + supported.brick)
    tree

  def cascadeCount(tree: Map[Brick, Set[Brick]], supported: Map[Brick, Supported]): Map[Brick, Int] =
    @tailrec
    def count(check: List[Brick], fallen: Set[Brick], acc: Int): Int =
      check match
        case Nil          => acc
        case head :: tail =>
          // Bricks supported by `head` might fall...
          val mightFall = tree.getOrElse(head, Set.empty).toList
          // If all the bricks supporting them have also fallen
          // but don't count a brick twice
          val falls = mightFall
            .filter: b =>
              supported(b).by.forall(fallen.contains(_))
            .filterNot: b =>
              fallen.contains(b)
          count(tail ::: falls, fallen ++ falls, acc + falls.size)

    Map.from(
      for brick <- tree.keys
      yield brick -> count(List(brick), Set(brick), 0),
    )

  def part2(input: Input) =
    val resting = fall(input)
    val tree    = cascadeTree(fall(input))
    val counts  = cascadeCount(tree, resting.map(x => x.brick -> x).toMap)
    counts.map(_._2).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
