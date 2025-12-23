package aoc2015

import scala.annotation.tailrec
import aoc.Day

object Day16 extends Day:

  type Input = List[Map[String, Int]]

  def parse(xs: List[String]): Input = xs.map:
    case s"Sue $_: $facts" =>
      facts
        .split(", ")
        .map:
          case s"$key: $value" => key -> value.toInt
        .toMap

  lazy val input = parse(loadInput().toList)

  val analysis = Map(
    "children"    -> 3,
    "cats"        -> 7,
    "samoyeds"    -> 2,
    "pomeranians" -> 3,
    "akitas"      -> 0,
    "vizslas"     -> 0,
    "goldfish"    -> 5,
    "trees"       -> 3,
    "cars"        -> 2,
    "perfumes"    -> 1,
  )

  lazy val part1 =
    input.zipWithIndex
      .filter: (facts, i) =>
        // For this Sue to be a match, all facts must be compatible with the analysis.
        facts.forall:
          case key -> value =>
            // If a Sue-fact is also in the analysis, the values must match;
            // otherwise this fact should be considered compatible (return true)
            analysis.get(key).map(_ == value).getOrElse(true)
      // We can probably assume there's only one match.
      .head
      ._2 + 1

  lazy val part2 =
    input.zipWithIndex
      .filter: (facts, i) =>
        facts.forall:
          case "cats" -> value        => analysis.get("cats").map(_ < value).getOrElse(true)
          case "trees" -> value       => analysis.get("trees").map(_ < value).getOrElse(true)
          case "pomeranians" -> value => analysis.get("pomeranians").map(_ > value).getOrElse(true)
          case "goldfish" -> value    => analysis.get("goldfish").map(_ > value).getOrElse(true)
          case key -> value           => analysis.get(key).map(_ == value).getOrElse(true)
      .head
      ._2 + 1

  final def main(args: Array[String]): Unit =
    solveP1(() => part1)
    solveP2(() => part2)
