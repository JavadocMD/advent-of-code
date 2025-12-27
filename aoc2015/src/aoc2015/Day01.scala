package aoc2015

import scala.annotation.tailrec
import aoc.Day

object Day01 extends Day:

  type Input = String

  def parse(xs: List[String]): Input = xs.head

  def part1(input: Input) = input.count(_ == '(') - input.count(_ == ')')

  def part2(input: Input) =
    case class State(floor: Long = 0, index: Int = 0)
    input.view
      .scanLeft(State()):
        case (State(f, i), '(') => State(f + 1, i + 1)
        case (State(f, i), ')') => State(f - 1, i + 1)
        case um                 => throw Unmatched(um)
      .find(_.floor == -1)
      .get
      .index

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
