package aoc2016

import scala.annotation.tailrec
import aoc.Day

object Day07 extends Day:

  lazy val input = loadInput()

  // Group 1 uses a positive lookahead to match overlapping ABBAs.
  val abbaPattern = """(?=(([a-z])(?!\2)([a-z])\3\2))""".r.unanchored

  def brackets(s: String): LazyList[Int] =
    // lazily computed bracket 'depth' at each index
    LazyList
      .from(s)
      .scanLeft(0):
        case (acc, '[') => acc + 1
        case (acc, ']') => acc - 1
        case (acc, _)   => acc
      .drop(1)

  def supportsTls(s: String): Boolean =
    val bs    = brackets(s)
    val abbas = abbaPattern.findAllMatchIn(s)
    abbas.nonEmpty && abbas.forall(m => bs(m.start) == 0)

  lazy val part1 = input.count(supportsTls)

  // Group 1 uses a positive lookahead to match overlapping ABAs.
  val abaPattern = """(?=(([a-z])(?!\2)([a-z])\2))""".r.unanchored

  def supportsSsl(s: String): Boolean =
    val bs      = brackets(s)
    val matches = abaPattern.findAllMatchIn(s).toList
    val abas    = matches.filter(m => bs(m.start) == 0).map(_.group(1))
    val babs    = matches.filter(m => bs(m.start) == 1).map(_.group(1))
    abas.exists: a =>
      val b = s"${a(1)}${a(0)}${a(1)}"
      babs.contains(b)

  lazy val part2 = input.count(supportsSsl)

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
