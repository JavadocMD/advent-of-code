package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day11 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  class Grid(input: Array[String]):
    val (w, h) = (input(0).size, input.size)

    private[this] var grid = input.map(_.toCharArray())

    def totalOccupied: Int =
      (for { row <- grid } yield row.count(_ == '#')).sum

    def isOccupied(x: Int, y: Int): Boolean =
      0 <= x && x < w &&
        0 <= y && y < h &&
        grid(y)(x) == '#'

    def occupied(x: Int, y: Int): Int =
      var count = 0
      for {
        j <- y - 1 to y + 1
        i <- x - 1 to x + 1
        if i != x || j != y
      } if (isOccupied(i, j)) count += 1
      count

    private[this] var gridEvolve = Array.ofDim[Char](h, w)

    def evolve(): Int =
      var changed = 0
      for {
        j <- 0 until h
        i <- 0 until w
      } {
        val curr = grid(j)(i)
        val next = curr match {
          case '.'                          => '.'
          case 'L' if (occupied(i, j) == 0) => changed += 1; '#'
          case 'L'                          => 'L'
          case '#' if (occupied(i, j) >= 4) => changed += 1; 'L'
          case '#'                          => '#'
          case _                            => throw new Exception(s"Unexpected seat state: $curr")
        }
        gridEvolve(j)(i) = next
      }
      val swap = grid
      grid = gridEvolve
      gridEvolve = swap
      changed

  end Grid

  def part1(input: Input): Int =
    val grid    = new Grid(input)
    var changed = -1
    while (changed != 0) {
      changed = grid.evolve()
    }
    grid.totalOccupied

  val directions = List(
    (+1, +0), // E
    (+1, +1), // SE
    (+0, +1), // S
    (-1, +1), // SW
    (-1, +0), // W
    (-1, -1), // NW
    (+0, -1), // N
    (+1, -1)  // NE
  )

  class Grid2(input: Array[String]):
    val (w, h)                  = (input(0).size, input.size)
    def idx(x: Int, y: Int)     = y * w + x
    def vec(i: Int): (Int, Int) = (i % w, i / w)

    private[this] var grid = input.mkString.toCharArray()

    def totalOccupied: Int = grid.count(_ == '#')

    def inBounds(x: Int, y: Int) = 0 <= x && x < w && 0 <= y && y < h

    def neighborsOf(x: Int, y: Int): List[Int] = {
      @tailrec
      def sight(dir: (Int, Int), curr: (Int, Int)): Option[Int] = {
        (dir, curr) match {
          case (_, (i, j)) if !inBounds(i, j)        => None
          case (_, (i, j)) if grid(idx(i, j)) == 'L' => Some(idx(i, j))
          case ((di, dj), (i, j))                    => sight(dir, (i + di, j + dj))
        }
      }
      directions.flatMap({ case dir @ (di, dj) => sight(dir, (x + di, y + dj)) })
    }

    val neighborsMap: Map[Int, List[Int]] = {
      val xs = for {
        j <- 0 to h
        i <- 0 to w
      } yield idx(i, j) -> neighborsOf(i, j)
      Map.from(xs)
    }

    def occupied(i: Int): Int = {
      neighborsMap(i).count(j => grid(j) == '#')
    }

    private[this] var gridEvolve = Array.from(grid)

    def evolve(): Int =
      var changed = 0
      for { i <- 0 until w * h } {
        val curr = grid(i)
        val next = curr match {
          case '.'                       => '.'
          case 'L' if (occupied(i) == 0) => changed += 1; '#'
          case 'L'                       => 'L'
          case '#' if (occupied(i) >= 5) => changed += 1; 'L'
          case '#'                       => '#'
          case _                         => throw new Exception(s"Unexpected seat state: $curr")
        }
        gridEvolve(i) = next
      }
      val swap = grid
      grid = gridEvolve
      gridEvolve = swap
      changed

  end Grid2

  def part2(input: Input): Int =
    val grid    = new Grid2(input)
    var changed = -1
    while (changed != 0) {
      changed = grid.evolve()
    }
    grid.totalOccupied

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
