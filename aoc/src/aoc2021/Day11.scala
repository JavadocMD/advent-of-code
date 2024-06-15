package aoc2021

import scala.annotation.tailrec
import scala.collection.View.Iterate
import Grid2D._

import aoc.Day

object Day11 extends Day:
  type Grid = Grid2D[Int]

  val parse = parseGrid(_.asDigit)

  type Step = (Int, Grid) // (how many flashed, the new grid state)

  def step(grid: Grid): Step =
    @tailrec
    def recurse(frontier: List[Point], acc: Grid, flashed: Set[Point] = Set.empty): (Set[Point], Grid) =
      frontier match
        case Nil => (flashed, acc)
        case p :: fs if flashed.contains(p) =>
          recurse(fs, acc, flashed)
        case p :: fs =>
          val energy = acc(p) + 1
          if (energy < 10)
            // No flash, just update value.
            recurse(fs, acc.updated(p, energy), flashed)
          else
            // Flash! Chain to neighbors and set energy to 0; ensure we skip this point in future.
            val chains = p.adjacent8.filter(q => acc.contains(q) && !flashed.contains(q))
            recurse(chains ::: fs, acc.updated(p, 0), flashed + p)

    val (flashed, result) = recurse(grid.keys.toList, grid)
    (flashed.size, result)

  type Flashes = LazyList[Int]

  def flashesFrom(grid: Grid): Flashes = LazyList.unfold(grid)(step andThen Some.apply)

  def part1(fs: Flashes) = fs.take(100).sum

  def part2(fs: Flashes) = fs.takeWhile(_ != 100).size + 1

  final def main(args: Array[String]): Unit =
    val in = flashesFrom(parse(loadInput()))
    solveP1(() => part1(in))
    solveP2(() => part2(in))
