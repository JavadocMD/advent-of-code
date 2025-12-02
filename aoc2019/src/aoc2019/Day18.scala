package aoc2019

import aoc._
import scala.annotation.tailrec

import aoc.Day

object Day18 extends Day:
  import TwoDee._

  // This solution heavily cribbed from: https://todd.ginsberg.com/post/advent-of-code/2019/day18/
  // I believe state caching (in the memoized bestMove below) was the primary missing piece.
  // With that disabled, this solution shows similar timing characteristics as my other solutions --
  // namely that, of the examples, only #4 takes unreasonably long.

  sealed trait Cell
  object Cell:
    case object Wall             extends Cell
    case object Empty            extends Cell
    case object Start            extends Cell
    case class Door(label: Char) extends Cell
    case class Key(label: Char)  extends Cell

    def parse(c: Char): Cell = c match
      case '#'                      => Wall
      case '.'                      => Empty
      case '@'                      => Start
      case c if 65 <= c && c <= 90  => Door(c.toLower)
      case c if 97 <= c && c <= 122 => Key(c)
      case c                        => throw new Exception(s"Unexpected char in map: ${c}")

  class Maze(val grid: Grid[Cell], val starts: Set[Vector], val keys: Map[Char, Vector]):
    val keyset = keys.keySet

  type Input = Maze
  def parse(xs: Array[String]): Input =
    val grid   = Grid.parse(Cell.Wall, Cell.parse)(xs)
    val starts = grid.filter({ case (_, c) => c == Cell.Start }).keySet
    val keys = grid.flatMap[Char, Vector]({
      case (v, Cell.Key(k)) => Some(k -> v)
      case _                => None
    })
    new Maze(grid, starts, keys)

  def solve(maze: Maze): Int =
    import scala.collection.{mutable => m}

    case class Step(key: Char, to: Vector, dist: Int)

    // Find Steps corresponding to the keys that we don't have that are reachable.
    def findKeys(from: Vector, haveKeys: Set[Char]): List[Step] = {
      var keys     = List.empty[Step]
      val distance = m.Map(from -> 0)
      val frontier = m.Queue(from)
      while (frontier.nonEmpty) {
        val prev = frontier.dequeue()
        for {
          curr <- prev.neighbors
          cell = maze.grid(curr)
          if cell != Cell.Wall && !distance.contains(curr)
        } {
          val dist = distance(prev) + 1
          distance(curr) = dist
          maze.grid(curr) match {
            case Cell.Empty                           => frontier += curr
            case Cell.Start                           => frontier += curr
            case Cell.Door(d) if haveKeys.contains(d) => frontier += curr
            case Cell.Key(k) if haveKeys.contains(k)  => frontier += curr
            case Cell.Key(k)                          => keys ::= Step(k, curr, dist)
            case _                                    => // ignore
          }
        }
      }
      keys
    }

    case class State(starts: Set[Vector], haveKeys: Set[Char])

    // Cache states we've already calculated.
    val bestMoveMemo = m.Map.empty[State, Int]
    // Find the best move for the given state, recursively.
    def bestMove(starts: Set[Vector], haveKeys: Set[Char]): Int = {
      def calculate: Int = {
        val stepScores = for {
          from                <- starts.toList
          Step(key, to, dist) <- findKeys(from, haveKeys)
          nextDist = bestMove(starts - from + to, haveKeys + key)
        } yield dist + nextDist
        stepScores.minOption.getOrElse(0)
      }
      bestMoveMemo.getOrElseUpdate(State(starts, haveKeys), calculate)
    }

    bestMove(maze.starts, Set.empty)
  end solve

  def part1(input: Input): Long = solve(input)

  def part2(input: Input): Long =
    // (input manually modified from above)
    solve(parse(load("aoc2019/Day18b.input.txt")))

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
