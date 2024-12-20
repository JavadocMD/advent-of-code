package aoc2024

import scala.annotation.tailrec
import aoc.Day

import aoc2024.Util._

object Day20 extends Day:

  type Input = (Grid[Char], Vector2, Vector2) // grid, start, finish

  def parse(xs: List[String]): Input =
    val grid = Map.from(
      for
        (line, y) <- xs.zipWithIndex
        (char, x) <- line.zipWithIndex
      yield Vector2(x, y) -> char
    )
    val start  = grid.find((_, x) => x == 'S').get._1
    val finish = grid.find((_, x) => x == 'E').get._1
    (grid.updated(start, '.').updated(finish, '.'), start, finish)

  def findCheats(grid: Grid[Char], fair: Path): Iterator[(Vector2, Vector2, Long)] =
    val Path(fairScore, fairPath) = fair
    val fairPathIndex             = fairPath.zipWithIndex.toMap
    val cellAt                    = grid.getOrElse(_, 'X')
    // I don't think there are any corners worth cutting... so just check cardinals
    for
      origin <- fairPath.toIterator
      dir    <- Direction.all
      target = origin + dir + dir
      if fairPathIndex.contains(target) && cellAt(origin + dir) == '#'
      cheatScore = fairPathIndex(origin) + (fairScore - fairPathIndex(target)) + 2
      if cheatScore < fairScore
    yield (origin, target, cheatScore)

  def part1(input: Input) =
    val (grid, start, finish) = input
    val fair                  = bfs(grid, grid(_) == '.', _ => 1L, start, finish).get
    findCheats(grid, fair)
      .filter:
        case (_, _, x) => fair.score - x >= 100
      .size

  def findCheats2(grid: Grid[Char], fair: Path): Iterator[(Vector2, Vector2, Long)] =
    val Path(fairScore, fairPath) = fair
    val fairPathIndex             = fairPath.zipWithIndex.toMap
    val cellAt                    = grid.getOrElse(_, 'X')
    for
      origin <- fairPath.toIterator
      target <- fairPath
      dist        = origin.manhattan(target) if dist > 1 && dist <= 20
      originSteps = fairPathIndex(origin)
      targetSteps = fairPathIndex(target)
      if targetSteps > originSteps // no going backwards
      cheatScore = originSteps + dist + (fairScore - targetSteps)
      if cheatScore < fairScore // cheat must save time
    yield (origin, target, cheatScore)

  def part2(input: Input) =
    val (grid, start, finish) = input
    val fair                  = bfs(grid, grid(_) == '.', _ => 1L, start, finish).get
    findCheats2(grid, fair)
      .filter:
        case (_, _, x) => fair.score - x >= 100
      .size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
