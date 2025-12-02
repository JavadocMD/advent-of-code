package aoc2021

import scala.annotation.tailrec

import aoc.Day

object Day10 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  val open  = List('(', '[', '{', '<')
  val close = List(')', ']', '}', '>')

  val scores1         = List(3, 57, 1197, 25137)
  def score1(c: Char) = scores1(close.indexOf(c))

  def matched(a: Char, b: Char): Boolean =
    open.indexOf(a) == close.indexOf(b)

  @tailrec
  def corruptionScore(line: List[Char], stack: List[Char] = List.empty): (Int, List[Char]) =
    line match
      case Nil => (0, stack)
      case opener :: lineTail if open.contains(opener) =>
        corruptionScore(lineTail, opener :: stack)
      case closer :: lineTail if close.contains(closer) =>
        if matched(stack.head, closer) then corruptionScore(lineTail, stack.tail)
        else (score1(closer), stack)
      case _ => throw new Exception("invalid case")

  def part1(input: Input) =
    val scores = for
      s <- input.toList
      (corr, _) = corruptionScore(s.toList)
    yield corr
    scores.sum

  val scores2         = List(1, 2, 3, 4)
  def score2(c: Char) = scores2.apply(open.indexOf(c))

  def autocompleteScore(stack: List[Char]): Long =
    stack.foldLeft(0L) { (acc, curr) =>
      5 * acc + score2(curr)
    }

  def part2(input: Input) =
    val scores = for
      s <- input.toList
      (corr, stack) = corruptionScore(s.toList)
      if corr == 0
    yield autocompleteScore(stack)
    scores.sorted.apply(scores.size / 2)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
