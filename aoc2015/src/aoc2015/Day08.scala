package aoc2015

import scala.annotation.tailrec
import aoc.Day

object Day08 extends Day:

  type Input = List[String]

  def parse(xs: List[String]): Input = xs

  def decode(s: String): String =
    @tailrec
    def recurse(s: List[Char], acc: List[Char] = Nil): List[Char] = s match
      case Nil                             => acc.reverse
      case '\\' :: 'x' :: n0 :: n1 :: tail => recurse(tail, '_' :: acc)
      case '\\' :: '\\' :: tail            => recurse(tail, '\\' :: acc)
      case '\\' :: '"' :: tail             => recurse(tail, '"' :: acc)
      case '"' :: tail                     => recurse(tail, acc)
      case head :: tail                    => recurse(tail, head :: acc)

    recurse(s.toList).mkString

  def part1(input: Input) =
    val literalSize = input.map(_.size).sum
    val memorySize  = input.map(decode(_).size).sum
    literalSize - memorySize

  def encode(s: String): String =
    @tailrec
    def recurse(s: List[Char], acc: List[Char] = Nil): List[Char] = s match
      case Nil          => '"' :: ('"' :: acc).reverse
      case '\\' :: tail => recurse(tail, '\\' :: '\\' :: acc)
      case '"' :: tail  => recurse(tail, '"' :: '\\' :: acc)
      case head :: tail => recurse(tail, head :: acc)

    recurse(s.toList).mkString

  def part2(input: Input) =
    val doubleEncSize = input.map(encode(_).size).sum
    val literalSize   = input.map(_.size).sum
    doubleEncSize - literalSize

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
