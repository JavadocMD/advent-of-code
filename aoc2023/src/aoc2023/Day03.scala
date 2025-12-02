package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day03 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  type Grid = IndexedSeq[IndexedSeq[Char]]

  def adjacent(coord: (Int, Int)): Seq[(Int, Int)] =
    coord match
      case (x, y) =>
        Seq(
          (x - 1, y - 1),
          (x, y - 1),
          (x + 1, y - 1),
          (x - 1, y),
          // (x, y),
          (x + 1, y),
          (x - 1, y + 1),
          (x, y + 1),
          (x + 1, y + 1),
        )

  def expand_number(grid: Grid, coord: (Int, Int)): (Long, Int, Int, Int) =
    val i      = coord._1
    val j      = coord._2
    var number = Seq(grid(j)(i))

    var n = i - 1
    while n >= 0 do
      val c = grid(j)(n)
      if c.isDigit then
        number +:= c
        n -= 1
      else n = -1

    var m = i + 1
    while m < 140 do
      val c = grid(j)(m)
      if c.isDigit then
        number :+= c
        m += 1
      else m = 140

    (number.mkString.toLong, n, m, j)

  def numbers_around(grid: Grid, coord: (Int, Int)): List[Long] =
    adjacent(coord).toSet
      .filter({ case (x, y) =>
        x >= 0 && x < 140 && y >= 0 && y < 140
      })
      .filter({ case (x, y) =>
        grid(y)(x).isDigit
      })
      .map({ case coord =>
        expand_number(grid, coord)
      })
      .toList
      .map(_._1)

  def part1(input: Input) =
    val grid =
      for line <- input.toIndexedSeq
      yield line.toIndexedSeq

    val symbols = for
      (line, j) <- input.view.zipWithIndex
      (char, i) <- line.zipWithIndex
      coord     <- if char.isDigit || char == '.' then None else Some((i, j))
    yield coord

    symbols.flatMap(numbers_around(grid, _)).sum

  def part2(input: Input) =
    val grid =
      for line <- input.toIndexedSeq
      yield line.toIndexedSeq

    val symbols = for
      (line, j) <- input.view.zipWithIndex
      (char, i) <- line.zipWithIndex
      coord     <- if char == '*' then Some((i, j)) else None
    yield coord

    symbols
      .map(numbers_around(grid, _))
      .filter(_.size == 2)
      .map(_.product)
      .sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
