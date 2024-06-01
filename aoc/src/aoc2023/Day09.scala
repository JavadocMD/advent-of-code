package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day09 extends Day:

  type Input = List[List[Long]]

  def parse(xs: List[String]): Input =
    xs.map(_.split(" ").view.map(_.toLong).toList)

  // def differentiate(xs: List[Long]): (List[Long], Boolean) =
  //   @tailrec def recurse(last: Long, xs: List[Long], dxs: List[Long], isZero: Boolean): (List[Long], Boolean) =
  //     xs match
  //       case Nil => (dxs.reverse, isZero)
  //       case head :: tail =>
  //         val dx = last - head
  //         recurse(head, tail, dx :: dxs, dx == 0 && isZero)
  //   recurse(xs.head, xs.tail, Nil, true)

  // def predict(numbers: List[Long]): Long =
  //   def recurse(xs: List[Long]): Long =
  //     val (dxs, isZero) = differentiate(xs)
  //     if isZero then xs.head
  //     else xs.head + recurse(dxs)
  //   recurse(numbers.reverse)

  def differentiate(xs: List[Long]): List[Long] =
    xs.lazyZip(xs.tail).map { case (a, b) => b - a }

  def predict(xs: List[Long]): Long =
    val dxs = differentiate(xs)
    if dxs.forall(_ == 0) then xs.last
    else xs.last + predict(dxs)

  def part1(input: Input) =
    input.map(predict).sum

  def part2(input: Input) =
    input.map(_.reverse).map(predict).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
