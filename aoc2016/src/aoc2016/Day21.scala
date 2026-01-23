package aoc2016

import scala.annotation.tailrec
import aoc.Day

object Day21 extends Day:

  lazy val input = loadInput()

  object I:
    def unapply(s: String): Option[Int] = s.toIntOption

  object C:
    def unapply(s: String): Option[Char] = s.headOption

  def swap(arr: Array[Char], i: Int, j: Int): Array[Char] =
    val result = arr.clone()
    result(i) = arr(j)
    result(j) = arr(i)
    result

  def rotate(arr: Array[Char], n: Int): Array[Char] =
    val result = arr.clone()
    val len    = arr.size
    for i <- 0 until len do result(i) = arr((len + len + i - n) % len)
    result

  def process(instructions: List[String], start: String): String =
    instructions
      .foldLeft(start.toArray):
        case (prev, s"swap position ${I(i)} with position ${I(j)}") => swap(prev, i, j)
        case (prev, s"swap letter ${C(x)} with letter ${C(y)}")     => swap(prev, prev.indexOf(x), prev.indexOf(y))
        case (prev, s"rotate left ${I(n)} step$_")                  => rotate(prev, -n)
        case (prev, s"rotate right ${I(n)} step$_")                 => rotate(prev, n)
        case (prev, s"rotate based on position of letter ${C(x)}") =>
          val i = prev.indexOf(x)
          val n = 1 + i + (if i >= 4 then 1 else 0)
          rotate(prev, n)
        case (prev, s"reverse positions ${I(i)} through ${I(j)}") =>
          val next = prev.clone()
          for (i, j) <- (i to j).zip(j to i by -1) do next(i) = prev(j)
          next
        case (prev, s"move position ${I(i)} to position ${I(j)}") =>
          val next = prev.clone()
          if i <= j then
            for k <- i to j - 1 do next(k) = prev(k + 1)
            next(j) = prev(i)
          else
            for k <- j + 1 to i do next(k) = prev(k - 1)
            next(j) = prev(i)
          next
        case (prev, x) => throw new Exception(s"Unsupported operation: $x")
      .mkString

  lazy val part1 = process(input, "abcdefgh")

  lazy val part2 =
    "abcdefgh".permutations
      .find(x => process(input, x) == "fbgdceah")
      .get

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
