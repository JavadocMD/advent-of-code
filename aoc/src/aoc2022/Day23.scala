package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day23 extends Day:
  case class Vector2(x: Int, y: Int):
    def n  = Vector2(x, y - 1)
    def ne = Vector2(x + 1, y - 1)
    def e  = Vector2(x + 1, y)
    def se = Vector2(x + 1, y + 1)
    def s  = Vector2(x, y + 1)
    def sw = Vector2(x - 1, y + 1)
    def w  = Vector2(x - 1, y)
    def nw = Vector2(x - 1, y - 1)
  end Vector2

  case class Elf(id: Int)

  enum Proposal:
    case Move(elf: Elf, from: Vector2)
    case Conflict

  type Grid[T] = Map[Vector2, T]

  type Input = Grid[Elf]

  def parse(xs: Iterator[String]): Input =
    val elfIds = Iterator.from(0)
    (for
      (line, j) <- xs.zipWithIndex
      (char, i) <- line.zipWithIndex
      if char == '#'
    yield Vector2(i, j) -> Elf(elfIds.next)).toMap

  type Consider = (Vector2, Grid[Elf]) => Option[Vector2]
  def considerNorth(p: Vector2, g: Grid[Elf]): Option[Vector2] =
    Option.when(!g.contains(p.n) && !g.contains(p.ne) && !g.contains(p.nw)) { p.n }
  def considerSouth(p: Vector2, g: Grid[Elf]): Option[Vector2] =
    Option.when(!g.contains(p.s) && !g.contains(p.se) && !g.contains(p.sw)) { p.s }
  def considerWest(p: Vector2, g: Grid[Elf]): Option[Vector2] =
    Option.when(!g.contains(p.w) && !g.contains(p.nw) && !g.contains(p.sw)) { p.w }
  def considerEast(p: Vector2, g: Grid[Elf]): Option[Vector2] =
    Option.when(!g.contains(p.e) && !g.contains(p.ne) && !g.contains(p.se)) { p.e }

  val considerables = List(considerNorth, considerSouth, considerWest, considerEast)

  def makeProposals(grid: Grid[Elf], consider: List[Consider]): Grid[Proposal] =
    val z = Map.empty[Vector2, Proposal]
    grid.foldLeft(z) { case (proposals, (position -> elf)) =>
      val cons = consider.flatMap(f => f(position, grid))
      if cons.size == 4 then proposals      // no move if all four sides are clear
      else if cons.size == 0 then proposals // no move if none are valid
      else                                  // propose moving to the first valid option
        val moveTo = cons.head
        proposals.get(moveTo) match
          case Some(_) => proposals + (moveTo -> Proposal.Conflict)
          case None    => proposals + (moveTo -> Proposal.Move(elf, position))
    }

  def commitProposals(prev: Grid[Elf], proposals: Grid[Proposal]): Grid[Elf] =
    proposals.foldLeft(prev) { case (nextGrid, (position -> prop)) =>
      prop match
        case Proposal.Move(elf, from) => nextGrid - from + (position -> elf)
        case Proposal.Conflict        => nextGrid
    }

  def rotateConsideration(consider: List[Consider]): List[Consider] = consider.tail :+ consider.head

  @tailrec
  def doRounds(grid: Grid[Elf], consider: List[Consider], steps: Int): Grid[Elf] =
    if steps == 0 then grid
    else
      val ps       = makeProposals(grid, consider)
      val nextGrid = commitProposals(grid, ps)
      val nextCons = rotateConsideration(consider)
      doRounds(nextGrid, nextCons, steps - 1)

  @tailrec
  def doRoundsUntilDone(grid: Grid[Elf], consider: List[Consider], rounds: Int = 1): Int =
    val ps       = makeProposals(grid, consider)
    val nextGrid = commitProposals(grid, ps)
    if nextGrid == grid then rounds
    else
      val nextCons = rotateConsideration(consider)
      doRoundsUntilDone(nextGrid, nextCons, rounds + 1)

  def bounds(grid: Grid[Elf]): (Vector2, Vector2) =
    var minX, minY = Int.MaxValue
    var maxX, maxY = Int.MinValue
    for p <- grid.keys do
      if p.x < minX then minX = p.x
      if p.x > maxX then maxX = p.x
      if p.y < minY then minY = p.y
      if p.y > maxY then maxY = p.y
    (Vector2(minX, minY), Vector2(maxX, maxY))

  def area(min: Vector2, max: Vector2): Int = (1 + max.x - min.x) * (1 + max.y - min.y)

  def stringify(grid: Grid[Elf], min: Vector2, max: Vector2): String =
    val rows =
      for j <- min.y to max.y
      yield
        val cols = for
          i <- min.x to max.x
          v = Vector2(i, j)
        yield if grid.contains(v) then '#' else '.'
        cols.mkString
    rows.mkString("\n")

  def part1(input: Input) =
    val finalGrid  = doRounds(input, considerables, 10)
    val (min, max) = bounds(finalGrid)
    area(min, max) - finalGrid.size // area of rectangle minus number of elves in it (all of the elves)

  def part2(input: Input) = doRoundsUntilDone(input, considerables)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
