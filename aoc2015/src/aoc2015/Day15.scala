package aoc2015

import scala.annotation.tailrec
import aoc.Day
import scala.math.max

object Day15 extends Day:

  case class Ingredients(name: String, cap: Int, dur: Int, flv: Int, tex: Int, cal: Int)

  type Input = List[Ingredients]

  def parse(xs: List[String]): Input = xs.map:
    case s"$name: capacity $cap, durability $dur, flavor $flv, texture $tex, calories $cal" =>
      Ingredients(name, cap.toInt, dur.toInt, flv.toInt, tex.toInt, cal.toInt)

  lazy val input = parse(loadInput().toList)

  def score(amounts: List[Int]): Long =
    (amounts zip input)
      .map:
        case (amt, ing) => List(amt * ing.cap, amt * ing.dur, amt * ing.flv, amt * ing.tex)
      .reduceLeft:
        case (as, bs) => (as zip bs).map(_ + _)
      .map(max(0, _).toLong)
      .product

  lazy val part1 =
    val results = for
      x0 <- (0 to 100).iterator
      x1 <- 0 to (100 - x0)
      x2 <- 0 to (100 - x0 - x1)
      x3      = 100 - x0 - x1 - x2
      amounts = List(x0, x1, x2, x3)
    yield score(amounts)
    results.max

  def calories(amounts: List[Int]): Long =
    (amounts zip input)
      .map:
        case (amt, ing) => amt * ing.cal
      .sum

  lazy val part2 =
    val results = for
      x0 <- (0 to 100).iterator
      x1 <- 0 to (100 - x0)
      x2 <- 0 to (100 - x0 - x1)
      x3      = 100 - x0 - x1 - x2
      amounts = List(x0, x1, x2, x3)
      if calories(amounts) == 500
    yield score(amounts)
    results.max

  final def main(args: Array[String]): Unit =
    solveP1(() => part1)
    solveP2(() => part2)
