package aoc2025

import scala.annotation.tailrec
import aoc.Day

object Day06 extends Day:

  type Input = List[String]

  def parse(xs: List[String]): Input = xs

  def part1(input: Input) =
    val columns = input.map(_.split("\\s+").toList).transpose
    columns.iterator
      .map(_.reverse) // put the operator at the front
      .map:
        case "*" :: nums => nums.map(_.toLong).reduce(_ * _)
        case "+" :: nums => nums.map(_.toLong).reduce(_ + _)
        case _           => throw new Exception("bad pattern")
      .sum

  @tailrec
  def calculateByColumns(xs: List[List[Char]], acc: Long = 0L): Long =
    // process the next group of columns; boundaries contain only spaces
    val (group, rest) = xs.span(_.exists(_ != ' '))
    if group.isEmpty then acc
    else
      // each column in the group is a number, but drop bottom char (operator or space)
      val nums = group.map(_.dropRight(1).mkString.trim.toLong)
      // the group's operator is the last char of first column
      val value = group.head.last match
        case '*' => nums.reduce(_ * _)
        case '+' => nums.reduce(_ + _)
        case _   => throw new Exception("op not found")
      // recurse after dropping the separator column
      calculateByColumns(rest.drop(1), acc + value)

  def part2(input: Input) =
    val columns = input.map(_.toList).transpose
    calculateByColumns(columns)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
