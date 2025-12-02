package aoc2019

import scala.annotation.tailrec
import scala.collection.mutable.ArrayDeque

import aoc.Day

object Day15 extends Day:
  import aoc2019.TwoDee._
  import aoc2019.IntcodeComputer as C

  type Input = C.Program
  def parse(xs: Array[String]): Input = C.parseMemory(xs.head)

  sealed trait Cell
  object Cell:
    case object Unknown extends Cell
    case object Wall    extends Cell
    case object Empty   extends Cell
    case object Start   extends Cell
    case object Goal    extends Cell

    def draw(c: Cell): String = c match {
      case Unknown => "?"
      case Wall    => "#"
      case Empty   => " "
      case Start   => "s"
      case Goal    => "x"
    }

    def isWalkable(c: Cell): Boolean = c match {
      case Unknown => false
      case Wall    => false
      case Empty   => true
      case Start   => true
      case Goal    => true
    }

  sealed trait Response
  object Response:
    case object HitWall   extends Response
    case object StepEmpty extends Response
    case object StepGoal  extends Response

    def parse(value: Long): Response = value match {
      case 0L => HitWall
      case 1L => StepEmpty
      case 2L => StepGoal
      case x  => throw new Exception(s"Invalid value for Response: ${x}")
    }

  def toMoveInput(d: Direction) = d match
    case Direction.North => 1L
    case Direction.South => 2L
    case Direction.West  => 3L
    case Direction.East  => 4L

  // Each step starts from a program State,
  // moves in Direction,
  // and ends at Vector
  type Step = (C.State, Vector, Direction)

  case class ExploreResult(grid: Grid[Cell], start: Vector, goal: Vector)

  def explore(program: C.Program): ExploreResult = {
    val initialState    = C.State(program, Iterator.empty)
    val initialPosition = Vector.zero

    var grid = Grid.create[Cell](Cell.Unknown).updated(initialPosition, Cell.Start)

    val horizon = new ArrayDeque[Step](64)

    def extendHorizon(state: C.State, position: Vector): Unit = {
      horizon ++= position.neighborsWithDir.map[Step]({ case (d, p) => (state, p, d) })
    }

    extendHorizon(initialState, initialPosition)

    // Explore the horizon.
    while (horizon.nonEmpty) {
      val (s0, p, d) = horizon.removeHead()
      if (!grid.isDefinedAt(p)) {
        val sf       = C.runIOSync1(s0, toMoveInput(d) :: Nil)
        val response = Response.parse(sf.output.head)
        response match {
          case Response.HitWall =>
            grid = grid.updated(p, Cell.Wall)

          case Response.StepEmpty =>
            grid = grid.updated(p, Cell.Empty)
            extendHorizon(sf, p)

          case Response.StepGoal =>
            grid = grid.updated(p, Cell.Goal)
            extendHorizon(sf, p)
        }
      }
    }

    grid.find({ case (v, c) => c == Cell.Goal }) match {
      case Some((goal, _)) => ExploreResult(grid, initialPosition, goal)
      case _               => throw new Exception("Goal not found.")
    }
  }

  def part1(input: Input): Long =
    val map0 = explore(input)
    val res0 = TwoDee.BreadthFirstSearch.create(map0.grid, Cell.isWalkable, map0.start)
    res0.distance(map0.goal)

  def part2(input: Input): Long =
    val map0 = explore(input)
    val res1 = TwoDee.BreadthFirstSearch.create(map0.grid, Cell.isWalkable, map0.goal)
    res1.distance.maxBy({ case (v, d) => d })._2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
