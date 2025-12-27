package aoc2015

import scala.annotation.tailrec
import aoc.Day

object Day10 extends Day:

  type Input = List[Char]

  def parse(xs: List[String]): Input = xs.head.toList

  def expand(digits: List[Char]): List[Char] =
    @tailrec
    def recurse(digits: List[Char], curr: Char, n: Int, acc: List[Char] = Nil): List[Char] =
      digits match
        case Nil                          => (curr :: (n.toString.toList ::: acc)).reverse
        case head :: tail if head == curr => recurse(tail, curr, n + 1, acc)
        case head :: tail                 => recurse(tail, head, 1, curr :: (n.toString.toList ::: acc))
    recurse(digits.tail, digits.head, 1)

  def part1(input: Input) =
    (0 until 40)
      .foldLeft(input):
        case (prev, _) => expand(prev)
      .size

  def part2(input: Input) =
    (0 until 50)
      .foldLeft(input):
        case (prev, _) => expand(prev)
      .size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
