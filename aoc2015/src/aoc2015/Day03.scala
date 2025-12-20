package aoc2015

import scala.annotation.tailrec
import aoc.Day
import aoc2015.Util._

object Day03 extends Day:

  type Input = List[Vector2]

  def parse(xs: List[String]): Input = xs.head.toList.collect:
    case '^' => Direction.N
    case 'v' => Direction.S
    case '>' => Direction.E
    case '<' => Direction.W

  def coords(moves: Iterator[Vector2]): Iterator[Vector2] = moves.iterator.scanLeft(Vector2(0, 0))(_ + _)

  def part1(input: Input) = coords(input.iterator).toSet.size

  def part2(input: Input) = (coords(input.every(2)) ++ coords(input.tail.every(2))).toSet.size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
