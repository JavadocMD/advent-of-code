package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day14 extends Day:

  type Tile  = '.' | 'O' | '#'
  type Grid  = List[List[Tile]]
  type Input = Grid

  def parse(xs: List[String]): Input =
    xs.map: line =>
      line.toList.map:
        case '.' => '.'
        case 'O' => 'O'
        case '#' => '#'

  def tiltLine(line: List[Tile]): List[Tile] =
    // For this tilt to work, the input must be pre-rotated 90 degrees counter-clockwise (left).
    // Then, a tilt happens in towards the north (unless of course you rotate after the initial one).
    def dots(n: Int) = List.fill[Tile](n)('.')
    @tailrec
    def recurse(ts: List[Tile], spaces: Int, acc: List[Char]): List[Char] =
      ts match
        case Nil         => (dots(spaces) ::: acc).reverse
        case '.' :: tail => recurse(tail, spaces + 1, acc)
        case 'O' :: tail => recurse(tail, spaces, 'O' :: acc)
        case '#' :: tail => recurse(tail, 0, '#' :: dots(spaces) ::: acc)
    recurse(line, 0, Nil).asInstanceOf[List[Tile]]

  def tilt(grid: Grid): Grid = grid.map(tiltLine)

  def scoreLine(line: List[Tile]): Long =
    val n = line.size
    line.view.zipWithIndex
      .filter((tile, _) => tile == 'O')
      .map((_, index) => n - index)
      .sum

  def score(grid: List[List[Tile]]): Long = grid.map(scoreLine).sum

  def stringify(grid: List[List[Tile]]): String =
    "\n" + grid.map(_.mkString).mkString("\n")

  def rot90left(grid: List[List[Tile]]): List[List[Tile]] =
    grid.transpose[Tile].reverse

  def rot90right(grid: List[List[Tile]]): List[List[Tile]] =
    grid.transpose[Tile].map(_.reverse)

  def part1(input: Input) = score(tilt(rot90left(input)))

  def spin(grid: Grid): Grid =
    var g = tilt(grid) // north
    g = tilt(rot90right(g)) // west
    g = tilt(rot90right(g)) // south
    g = tilt(rot90right(g)) // east
    rot90right(g)           // back to north

  def spinCycle(grid: Grid): LazyList[Grid] =
    lazy val xs: LazyList[Grid] = spin(grid) #:: xs.map(spin)
    xs

  def spinCycleScore(grid: Grid): LazyList[Long] = spinCycle(grid).map(score)

  // A cycle, where each tuple is the item and the index where it was first found
  type Cycle[T] = List[(T, Int)]

  def detectCycle[T](sequence: LazyList[T]): Cycle[T] =
    // Assumes sequence is infinite.
    @tailrec
    def recurse(xs: LazyList[(T, Int)], seen: Set[T], cycle: Vector[(T, Int)]): Cycle[T] =
      val (s, i) = xs.head
      // Hmm... this item was at the start of our current cycle.
      if s == cycle.head._1 then
        // Cycle is greater than one! Accept it.
        if cycle.size > 1 then cycle.toList
        // Otherwise reset the current cycle.
        else recurse(xs.tail, seen + s, Vector(xs.head))
      // We've seen this value, but it's not the start of the cycle. Add it!
      else if seen.contains(s) then recurse(xs.tail, seen, cycle :+ xs.head)
      // Ah, new value, reset the cycle.
      else recurse(xs.tail, seen + s, Vector(xs.head))

    val indexed = sequence.zipWithIndex
    recurse(indexed.tail, Set(indexed.head._1), Vector(indexed.head))

  def cycleIndex[T](cycle: Cycle[T]): Int => T =
    val start = cycle.map(_._2).min
    val size  = cycle.size
    (i: Int) =>
      if i < start then throw Exception("index comes before cycle")
      else cycle((i - start) % size)._1

  def part2(input: Input) =
    // Doing some println investigation lead me to discover that there's a cycle not too far in!
    // Just have to detect it and index into it.

    // var grid     = rot90left(input)
    // var scoreMap = Map.empty[Long, List[Int]] // score -> which step did we get this score?
    // for i <- 1 until 300 do
    //   grid = spin(grid)
    //   val s     = score(grid)
    //   val steps = scoreMap.get(s).getOrElse(List.empty)
    //   scoreMap = scoreMap.updated(s, i :: steps)

    // val string = scoreMap.toList
    //   .sortBy((score, steps) => steps.min)
    //   .map((score, steps) => s"$score : ${steps.sorted}")
    //   .mkString("\n")
    // println(string)

    val scores = spinCycleScore(rot90left(input))
    val cycle  = detectCycle(scores)
    cycleIndex(cycle)(1_000_000_000 - 1)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
