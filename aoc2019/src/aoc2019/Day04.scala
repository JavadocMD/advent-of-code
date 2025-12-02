package aoc2019

import scala.io.Source
import scala.annotation.tailrec
import fastparse._
import NoWhitespace._

import aoc.Day

object Day04 extends Day:
  type Input = (Int, Int)
  def parse(xs: Array[String]): Input = xs.map { case s"${min}-${max}" =>
    (min.toInt, max.toInt)
  }.head

  type PasswordFilter = (Int, Int, Int, Int, Int, Int) => Boolean

  def hasDouble(d0: Int, d1: Int, d2: Int, d3: Int, d4: Int, d5: Int): Boolean =
    d0 == d1 || d1 == d2 || d2 == d3 || d3 == d4 || d4 == d5

  def hasLimitedDouble(d0: Int, d1: Int, d2: Int, d3: Int, d4: Int, d5: Int): Boolean = {
    (d0 == d1 && d1 != d2) ||
    (d0 != d1 && d1 == d2 && d2 != d3) ||
    (d1 != d2 && d2 == d3 && d3 != d4) ||
    (d2 != d3 && d3 == d4 && d4 != d5) ||
    (d3 != d4 && d4 == d5)
  }

  def generatePasswords(filter: PasswordFilter): Seq[Int] = for {
    d0 <- 1 to 9
    d1 <- d0 to 9
    d2 <- d1 to 9
    d3 <- d2 to 9
    d4 <- d3 to 9
    d5 <- d4 to 9
    if filter(d0, d1, d2, d3, d4, d5)
  } yield s"$d0$d1$d2$d3$d4$d5".toInt

  def part1(input: Input): Long =
    val (min, max) = input
    generatePasswords(hasDouble)
      .dropWhile(n => n < min)
      .takeWhile(n => n <= max)
      .size

  def part2(input: Input): Long =
    val (min, max) = input
    generatePasswords(hasLimitedDouble)
      .dropWhile(n => n < min)
      .takeWhile(n => n <= max)
      .size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
