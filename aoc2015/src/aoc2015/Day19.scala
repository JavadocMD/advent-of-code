package aoc2015

import scala.annotation.tailrec
import aoc.Day
import scala.math.{min, max, abs}
import aoc2015.Util._
import scala.util.Random

object Day19 extends Day:

  case class Replacement(src: String, dst: String)

  case class Input(repls: List[Replacement], molecule: String)

  def parse(xs: List[String]): Input =
    val rs :: ms :: Nil = chunkify(xs): @unchecked
    val repls = rs.map:
      case s"$src => $dst" => Replacement(src, dst)
    Input(repls, ms.head)

  lazy val input = parse(loadInput().toList)

  lazy val part1 =
    val molecule = input.molecule
    input.repls.iterator
      .flatMap: r =>
        r.src.r.unanchored
          .findAllMatchIn(molecule)
          .map: m =>
            val prefix = molecule.substring(0, m.start)
            val suffix = molecule.substring(m.end)
            s"$prefix${r.dst}$suffix"
      .toSet
      .size

  lazy val part2 =
    // This one's really unsatisfying, but I guess if you get the replacements
    // in the right priority order, you'll get the correct answer if you just
    // do the first replacement possible until you get to "e".
    // The fastest way to get to the correct order is apparently to random shuffle
    // and try again if you get stuck.

    @tailrec
    def reduce(molecule: String, repls: List[Replacement], steps: Int = 0): Option[Int] =
      if molecule == "e" then Some(steps)
      else
        val next = repls.iterator
          .flatMap: r =>
            r.src.r
              .findFirstMatchIn(molecule)
              .map: m =>
                val prefix = molecule.substring(0, m.start)
                val suffix = molecule.substring(m.end)
                s"$prefix${r.dst}$suffix"
          .nextOption
        next match
          case None    => None
          case Some(x) => reduce(x, repls, steps + 1)

    val repls       = input.repls.map(r => Replacement(r.dst, r.src))
    def replsRandom = Random.shuffle(repls)

    def everyDayImShufflin: Int = reduce(input.molecule, replsRandom) match
      case Some(steps) => steps
      case None        => everyDayImShufflin

    everyDayImShufflin

  final def main(args: Array[String]): Unit =
    input // evaluate input so parse time doesn't get counted
    solveP1(() => part1)
    solveP2(() => part2)
