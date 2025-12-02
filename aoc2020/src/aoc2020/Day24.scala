package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day24 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  sealed trait Direction
  object Direction:
    case object East      extends Direction
    case object Northeast extends Direction
    case object Southeast extends Direction
    case object West      extends Direction
    case object Northwest extends Direction
    case object Southwest extends Direction

    def iterator(s: String): Iterator[Direction] =
      val in = s.iterator
      new Iterator[Direction]() {
        override def hasNext: Boolean = in.hasNext
        override def next(): Direction = {
          in.next() match {
            case 'e' => Direction.East
            case 'w' => Direction.West
            case 'n' =>
              in.next() match {
                case 'e' => Direction.Northeast
                case 'w' => Direction.Northwest
              }
            case 's' =>
              in.next() match {
                case 'e' => Direction.Southeast
                case 'w' => Direction.Southwest
              }
          }
        }
      }

  end Direction

  case class Vector3(x: Int, y: Int, z: Int):
    def move(d: Direction): Vector3 = d match {
      case Direction.East      => Vector3(x + 1, y - 1, z)
      case Direction.Northeast => Vector3(x + 1, y, z - 1)
      case Direction.Southeast => Vector3(x, y - 1, z + 1)
      case Direction.West      => Vector3(x - 1, y + 1, z)
      case Direction.Northwest => Vector3(x, y + 1, z - 1)
      case Direction.Southwest => Vector3(x - 1, y, z + 1)
    }

    def neighbors: Seq[Vector3] = Seq(
      Vector3(x + 1, y - 1, z),
      Vector3(x + 1, y, z - 1),
      Vector3(x, y - 1, z + 1),
      Vector3(x - 1, y + 1, z),
      Vector3(x, y + 1, z - 1),
      Vector3(x - 1, y, z + 1)
    )

  end Vector3

  object Vector3:
    val Zero = Vector3(0, 0, 0)

  type TileMap = Set[Vector3] // Only store black tiles.

  def setupTiles(input: Array[String]): TileMap =
    var ts = Set.empty[Vector3]
    for (s <- input) {
      var curr = Vector3.Zero
      for (d <- Direction.iterator(s)) {
        curr = curr.move(d)
      }
      if (ts.contains(curr)) ts -= curr
      else ts += curr
    }
    ts

  def part1(input: Input): Int =
    var tiles = setupTiles(input)
    tiles.size

  def evolve(tiles: TileMap): TileMap =
    // How many black neighbors does each tile have?
    var nsBlack = Map.empty[Vector3, Int]
    for (v <- tiles; n <- v.neighbors) {
      val count = nsBlack.getOrElse(n, 0)
      nsBlack = nsBlack.updated(n, count + 1)
    }
    // Apply rules for each tile with any black neighbors.
    var result = Set.empty[Vector3]
    for ((v, count) <- nsBlack) {
      if (tiles.contains(v)) {
        // Black
        if (count <= 2) result += v
      } else {
        // White
        if (count == 2) result += v
      }
    }
    result

  def part2(input: Input): Int =
    var tiles = setupTiles(input)
    for (i <- 0 until 100) {
      tiles = evolve(tiles)
    }
    tiles.size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
