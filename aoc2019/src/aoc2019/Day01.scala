package aoc2019

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day01 extends Day:
  type Input = Array[Long]
  def parse(xs: Array[String]): Input = xs.map(_.toLong)

  def calculateFuel(mass: Long): Long = Math.max((mass / 3) - 2, 0)

  @tailrec
  def calculateFuelFuel(fuel: Long, total: Long = 0): Long =
    val stepFuel = calculateFuel(fuel)
    if (stepFuel == 0) total
    else calculateFuelFuel(stepFuel, total + stepFuel)

  def part1(input: Input): Long = input.map(calculateFuel).sum

  def part2(input: Input): Long = input.map(x => calculateFuelFuel(x, 0)).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
