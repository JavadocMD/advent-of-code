package aoc2015

import scala.annotation.tailrec
import aoc.Day

object Day24 extends Day:

  type Input = List[Long]

  def parse(xs: List[String]): Input = xs.map(_.toLong)

  lazy val input = parse(loadInput().toList)

  lazy val totalWeight = input.sum

  @tailrec
  def arrangements(otherCompartments: Int, size: Int = 1): Iterator[List[Long]] =
    // Generate all combinations (of the given size) for packages placed in the passenger compartment,
    // where the weight in the passenger compartment can equal the weights of the other compartments combined.

    // I don't think this is a general solution, actually, because it doesn't prove
    // that the remaining packages can be divided equally, but it works for this input.
    val validArrangements = for
      inPassenger <- input.combinations(size)
      passengerWeight = inPassenger.sum
      if passengerWeight * otherCompartments == (totalWeight - passengerWeight)
    yield inPassenger
    if validArrangements.hasNext then validArrangements
    else arrangements(otherCompartments, size + 1)

  lazy val part1 = arrangements(2).map(_.product).min

  lazy val part2 = arrangements(3).map(_.product).min

  final def main(args: Array[String]): Unit =
    input
    solveP1(() => part1)
    solveP2(() => part2)
