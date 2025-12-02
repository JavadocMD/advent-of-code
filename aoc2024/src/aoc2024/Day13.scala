package aoc2024

import scala.annotation.tailrec
import aoc.Day
import org.apache.commons.math3.linear.{Array2DRowRealMatrix as Matrix}
import org.apache.commons.math3.linear.LUDecomposition
import org.apache.commons.math3.linear.{ArrayRealVector as Vector}

object Day13 extends Day:

  case class Vector2(x: Long, y: Long):
    def +(that: Vector2): Vector2    = Vector2(this.x + that.x, this.y + that.y)
    def toDoubleArray: Array[Double] = Array(this.x.toDouble, this.y.toDouble)

  object Vector2:
    def s(x: String, y: String): Vector2 = Vector2(x.toLong, y.toLong)

  case class Machine(a: Vector2, b: Vector2, prize: Vector2)

  type Input = List[Machine]

  def parse(xs: List[String]): Input =
    xs.filterNot(_.isBlank)
      .grouped(3)
      .collect:
        case List(
              s"Button A: X+$ax, Y+$ay",
              s"Button B: X+$bx, Y+$by",
              s"Prize: X=$px, Y=$py",
            ) =>
          Machine(Vector2.s(ax, ay), Vector2.s(bx, by), Vector2.s(px, py))
        case x => throw Exception(s"Unexpected case: $x")
      .toList

  def isInteger(x: Double): Boolean = (x - x.round.toDouble).abs < 1e-3
  // took some experimentation and the examples to land on 1e-3
  // 1e-9 is too precise and drops valid results

  def solve(machine: Machine): Option[Vector2] =
    val coeff = Array(
      machine.a.toDoubleArray,
      machine.b.toDoubleArray,
    ).transpose

    val const = machine.prize.toDoubleArray

    val Array(x, y) = LUDecomposition(Matrix(coeff, false))
      .getSolver()
      .solve(Vector(const, false))
      .toArray()

    // Solutions with negative or non-integer button presses are invalid here.
    if x < 0 || y < 0 then None
    else if !isInteger(x) || !isInteger(y) then None
    else Some(Vector2(x.round, y.round))

  def part1(input: Input) =
    input
      .flatMap(solve)
      .filter(p => p.x <= 100 && p.y <= 100)
      .map(p => 3 * p.x + p.y)
      .sum

  def part2(input: Input) =
    val offset = Vector2(10000000000000L, 10000000000000L)
    input
      .map: m =>
        m.copy(prize = m.prize + offset)
      .flatMap(solve)
      .map(p => 3 * p.x + p.y)
      .sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
