package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day07 extends Day:

  case class Equation(result: Long, values: List[Long]):
    val ops: Int = values.size - 1

  type Input = List[Equation]

  def parse(xs: List[String]): Input =
    xs.map:
      case s"$result: $values" => Equation(result.toLong, values.split(" ").toList.map(_.toInt))

  /** All possible combinations of operators, where each set is expressed as a list of integers. '0' for the first
    * operator (sum), '1' for the second operator (multiply), etc.
    */
  def operatorCombinations(operators: Int, length: Int): Iterator[List[Char]] =
    Iterator
      .range(0, math.pow(operators, length).toInt)
      .map: x =>
        val ops = java.lang.Integer.toString(x, operators)
        ("0" * (length - ops.size) + ops).toList

  @tailrec
  def check(result: Long, values: List[Long], ops: List[Char], acc: Long): Boolean =
    values match
      case Nil               => result == acc
      case _ if acc > result => false // ops only increase acc, so quit if acc > result
      case curr :: nextValues =>
        val nextAcc = ops.head match
          case '0' => acc + curr
          case '1' => acc * curr
          case '2' => (acc.toString + curr.toString).toLong
        check(result, nextValues, ops.tail, nextAcc)

  def resolves(eq: Equation): Boolean =
    operatorCombinations(2, eq.ops).exists: ops =>
      check(eq.result, eq.values.tail, ops, eq.values.head)

  def part1(input: Input) =
    input.iterator.filter(resolves).map(_.result).sum

  def resolves2(eq: Equation): Boolean =
    operatorCombinations(3, eq.ops).exists: ops =>
      check(eq.result, eq.values.tail, ops, eq.values.head)

  def part2(input: Input) =
    input.iterator.filter(eq => resolves(eq) || resolves2(eq)).map(_.result).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
