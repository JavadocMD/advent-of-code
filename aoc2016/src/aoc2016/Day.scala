package aoc

import scala.io.Source
import scala.util.matching.Regex
import scala.annotation.tailrec

val dayRegex: Regex  = "^Day(\\d{2}).*$".r
val yearRegex: Regex = "^aoc(\\d{4})$".r

// We can use EOF to provide multi-line answer values in the rare case that happens.
// EOF can be used in both values, either value, or neither -- so we have four cases.
val answers1 = "(?ms)\\d{2}:<<EOF\\n(.+?)\\nEOF,<<EOF\\n(.+?)\\nEOF".r
val answers2 = "(?ms)\\d{2}:<<EOF\\n(.+?)\\nEOF,(.+?)".r
val answers3 = "(?ms)\\d{2}:(.+?),<<EOF\\n(.+?)\\nEOF".r
val answers4 = "\\d{2}:(.+?),(.+?)".r

enum Part:
  case One, Two

abstract class Day:

  def day: Int = this.getClass.getSimpleName match
    case dayRegex(d) => d.toInt
    case _           => throw new IllegalArgumentException("Unable to extract day.")

  def year: Int = this.getClass.getPackageName match
    case yearRegex(y) => y.toInt
    case _            => throw new IllegalArgumentException("Unable to extract year.")

  def load(resource: String): List[String] =
    val source = Source.fromResource(resource)
    try source.getLines.toList
    finally source.close()

  def loadInput(): List[String] = load(f"aoc$year%04d/Day$day%02d.input.txt")

  /** Groups input lines into chunks, as separated by a blank space. */
  def chunkify(lines: List[String]): List[List[String]] =
    @tailrec
    def recurse(xs: List[String], acc: List[List[String]] = Nil): List[List[String]] =
      val (chunk, rest) = xs.span(_ != "")
      if chunk.isEmpty then acc.reverse
      else recurse(rest.drop(1), chunk :: acc)
    recurse(lines)

  lazy val answers: Option[(String, String)] =
    val lines = load(f"aoc$year%04d/Answers.txt").view
      .dropWhile(!_.startsWith(f"${day}%02d:"))
      .takeWhile(!_.startsWith(f"${day + 1}%02d:"))
      .mkString("\n")
    lines match {
      case answers1(a1, a2) => Some((a1, a2))
      case answers2(a1, a2) => Some((a1, a2))
      case answers3(a1, a2) => Some((a1, a2))
      case answers4(a1, a2) => Some((a1, a2))
      case _                => None
    }

  def solve[T](part: Part, solution: () => T): T =
    val t0      = System.currentTimeMillis()
    val soln    = solution()
    val solnStr = soln.toString
    val t1      = System.currentTimeMillis()

    val ans = (answers, part) match
      case (None, _)                => None
      case (Some((x, _)), Part.One) => Some(x)
      case (Some((_, x)), Part.Two) => Some(x)

    if (ans.isDefined) && (ans.get != solnStr) then
      val msg = s"Solution for Part $part did not match expected.\nExpected: ${ans.get}\nGot: $solnStr"
      throw new Exception(msg)

    val ansChecked = ans.map(_ => "✔ ").getOrElse("➖ ")
    println(s"$ansChecked $solnStr [${t1 - t0} ms]")
    soln

  def solveP1[T](solution: () => T): T = solve(Part.One, solution)

  def solveP2[T](solution: () => T): T = solve(Part.Two, solution)

  def main(args: Array[String]): Unit
