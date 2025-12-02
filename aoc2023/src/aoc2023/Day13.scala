package aoc2023

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import aoc.Day

object Day13 extends Day:

  type Input = List[List[String]]

  def parse(xs: List[String]): Input =
    @tailrec
    def recurse(lines: List[String], acc: List[List[String]]): List[List[String]] =
      val (block, rest) = lines.span(_.nonEmpty)
      rest match
        case Nil => (block :: acc).reverse
        case _   => recurse(rest.tail, block :: acc)
    recurse(xs, Nil)

  def findMirror(lines: List[String]): Option[Int] =
    (1 until lines.size).find: numberLeft =>
      val (left, right) = lines.splitAt(numberLeft)
      val n             = math.min(left.size, right.size)
      left.reverse.take(n) == right.take(n)

  def levenshtein(a: String, b: String): Int = (a zip b).count(_ != _)

  def findMirrorSmudge(lines: List[String]): Option[Int] =
    (1 until lines.size).find: numberLeft =>
      val (left, right) = lines.splitAt(numberLeft)
      val diffs         = (left.reverse zip right).filter(_ != _)
      diffs match
        case (a, b) :: Nil => levenshtein(a, b) == 1
        case _             => false

  def flip(lines: List[String]): List[String] = lines.transpose.map(_.mkString)

  def score(lines: List[String], method: List[String] => Option[Int]): Int =
    100 * method(lines).getOrElse(0) + method(flip(lines)).getOrElse(0)

  def part1(input: Input) =
    input.map(score(_, findMirror)).sum

  def part2(input: Input) =
    input.map(score(_, findMirrorSmudge)).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
