package aoc2025

import scala.annotation.tailrec
import aoc.Day

object Day01 extends Day:

  type Input = List[Int]

  def parse(xs: List[String]): Input = xs.map:
    case s"R$x" => x.toInt
    case s"L$x" => -x.toInt

  // Scala's modulo deals with negative numbers like positives that keep the sign:
  // >>> -1 % 10 == -1
  // But for this problem it's convenient for modulo to "wrap":
  // >>> mathModulo(-1, 10) == 9
  def mathModulo(number: Int, modulus: Int): Int = ((number % modulus) + modulus) % modulus

  def part1(input: Input) =
    val (_, zeros) = input.foldLeft((50, 0L)):
      case ((pos, zeros), x) =>
        val nextPos   = mathModulo(pos + x, 100)
        val nextZeros = zeros + (if nextPos == 0 then 1 else 0)
        (nextPos, nextZeros)

    zeros

  def part2(input: Input) =
    val (_, zeros) = input.foldLeft((50, 0L)):
      // Case: turning right by x steps
      // count complete spins where starting position contributes
      case ((pos, zeros), x) if x >= 0 =>
        val nextPos   = mathModulo(pos + x, 100)
        val nextZeros = zeros + (pos + x) / 100
        (nextPos, nextZeros)

      // Case: turning left by x steps
      // count complete spins where starting position detracts
      // plus one if we didn't start at zero but had enough travel to hit zero
      case ((pos, zeros), x) =>
        val nextPos = mathModulo(pos + x, 100)
        val nextZeros = zeros
          + (-x - pos) / 100
          + (if pos != 0 && -x >= pos then 1 else 0)
        (nextPos, nextZeros)

    zeros

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
