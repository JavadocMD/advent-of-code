package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day24 extends Day:
  case class Vector2(x: Int, y: Int):
    def minus(other: Vector2) = Vector2(x - other.x, y - other.y)
    def plus(other: Vector2)  = Vector2(x + other.x, y + other.y)
    def manhattanLength       = x.abs + y.abs
  object Vector2:
    val zero = Vector2(0, 0)

    def up(v: Vector2)    = Vector2(v.x, v.y - 1)
    def down(v: Vector2)  = Vector2(v.x, v.y + 1)
    def left(v: Vector2)  = Vector2(v.x - 1, v.y)
    def right(v: Vector2) = Vector2(v.x + 1, v.y)
  end Vector2

  case class Blizzard(position: Vector2, char: Char, next: Vector2 => Vector2):
    def evolve = copy(position = next(position))

  case class Input(size: Vector2, start: Vector2, goal: Vector2, blizzards: List[Blizzard]):
    def inMap(v: Vector2): Boolean = (0 <= v.x && v.x < size.x && 0 <= v.y && v.y < size.y) ||
      v == start || v == goal
    val blizzardsByStep = LazyList.iterate(blizzards)(_.map(_.evolve))
    val closedByStep    = blizzardsByStep.map(bs => Set.from(bs.iterator.map(_.position)))

  def parse(xs: Iterator[String]): Input =
    import Math.{floorMod => fmod}
    val all  = xs.toList
    val size = Vector2(all.head.size - 2, all.size - 2)

    val right = (v: Vector2) => Vector2(fmod(v.x + 1, size.x), v.y)
    val left  = (v: Vector2) => Vector2(fmod(v.x - 1, size.x), v.y)
    val down  = (v: Vector2) => Vector2(v.x, fmod(v.y + 1, size.y))
    val up    = (v: Vector2) => Vector2(v.x, fmod(v.y - 1, size.y))

    val blizzards = for
      (line, j) <- all.zipWithIndex
      (char, i) <- line.zipWithIndex
      v = Vector2(i - 1, j - 1)
      if char != '#' && char != '.'
    yield char match
      case '>' => Blizzard(v, char, right)
      case '<' => Blizzard(v, char, left)
      case '^' => Blizzard(v, char, up)
      case 'v' => Blizzard(v, char, down)
    val start = Vector2(all.head.indexOf('.') - 1, -1)
    val end   = Vector2(all.last.indexOf('.') - 1, all.size - 2)
    Input(size, start, end, blizzards)

  def stringify(input: Input, blizzards: List[Blizzard]): String =
    var cells = blizzards.map { b => b.position -> b.char }.toMap
    val rows = for j <- 0 until input.size.y yield
      val cols = for i <- 0 until input.size.x yield cells.get(Vector2(i, j)) match
        case Some(char) => char
        case None       => '.'
      cols.mkString
    rows.mkString("\n")

  def part1(input: Input) =
    val pathfind = Pathfind(input)
    pathfind.search(input.start, 0, input.goal)

  def part2(input: Input) =
    val pathfind = Pathfind(input)
    val d1       = pathfind.search(input.start, 0, input.goal)
    val d2       = pathfind.search(input.goal, d1, input.start)
    val d3       = pathfind.search(input.start, d2, input.goal)
    d3

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))

  class Pathfind(input: Input):
    import scala.collection.{mutable => m}

    case class Step(position: Vector2, step: Int, score: Int)
    given Ordering[Step] with
      def compare(a: Step, b: Step) = b.score.compare(a.score)

    def search(start: Vector2, startStep: Int, goal: Vector2): Int =
      val frontier = m.PriorityQueue(Step(start, startStep, 0))
      val visited  = m.Set((start, 0))

      // neighbors includes "wait" as an option
      def neighbors(v: Vector2) = List(Vector2.up(v), Vector2.down(v), Vector2.left(v), Vector2.right(v), v)
      def heuristic(v: Vector2) = goal.minus(v).manhattanLength
      def isWalkableNow(closed: Set[Vector2], step: Int)(v: Vector2): Boolean =
        input.inMap(v) && !visited.contains((v, step)) && !closed.contains(v)

      @tailrec def loop: Int =
        val Step(curr, currStep, currscore) = frontier.dequeue
        if curr == goal then currStep
        else
          val nextStep   = currStep + 1
          val isWalkable = isWalkableNow(input.closedByStep(nextStep), nextStep)
          for n <- neighbors(curr) if isWalkable(n) do
            frontier += Step(n, nextStep, nextStep + heuristic(n))
            visited += ((n, nextStep))
          loop
      end loop
      loop
  end Pathfind
