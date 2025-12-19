package aoc2025

import scala.annotation.tailrec
import aoc.Day
import aoc2025.Util.Vector3
import aoc2025.Util.lastOption

object Day08 extends Day:

  type Circuits = Vector[Set[Vector3]]
  case class Pair(a: Vector3, b: Vector3)
  case class State(circuits: Circuits, pairs: List[Pair])

  type Input = State

  def distSquare(a: Vector3, b: Vector3): Long =
    val dx = b.x.toLong - a.x
    val dy = b.y.toLong - a.y
    val dz = b.z.toLong - a.z
    dx * dx + dy * dy + dz * dz

  def parse(xs: List[String]): Input =
    val points = xs.map:
      case s"$x,$y,$z" => Vector3(x.toInt, y.toInt, z.toInt)

    // The initial set of circuits is every junction box on its own
    val initialCircuits = points.map(Set(_)).toVector

    // Compute pairwise distances for the matrix triangle (off-diagonal)
    // then sort, shortest distances first.
    val distancePairs = List
      .from(
        for
          ps <- points.tails if ps.nonEmpty
          a = ps.head
          b <- ps.tail
        yield Pair(a, b)
      )
      .sortBy(p => distSquare(p.a, p.b))

    // It turned out both parts benefited from computing both of these,
    // so I moved them here and made them part of the input
    State(initialCircuits, distancePairs)

  // Compute the new set of circuits after connecting the given pair
  def connect(circuits: Circuits, pair: Pair): Circuits =
    val Pair(a, b) = pair
    val ac         = circuits.find(_.contains(a)).get
    val bc         = circuits.find(_.contains(b)).get
    circuits.filterNot(x => x == ac || x == bc).appended(ac | bc)

  def part1(input: Input) =
    input.pairs
      .take(1000)
      .foldLeft(input.circuits)(connect)
      .sortBy(_.size)(Ordering[Int].reverse)
      .take(3)
      .map(_.size)
      .product

  def part2(input: Input) =
    // Connect boxes in least-distance order, stop when there is only one circuit
    // and return the sequence of pairs connected along the way
    // Unfold state is: (current circuits, remaining pairs)
    val connections = Iterator.unfold(input):
      case State(cs, p :: tail) if cs.size > 1 => Some((p, State(connect(cs, p), tail)))
      case State(cs, ps)                       => None

    // We only care about the last pair
    val Pair(a, b) = lastOption(connections).get
    a.x.toLong * b.x

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
