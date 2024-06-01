package aoc

import scala.io.Source
import scala.util.matching.Regex

val dayRegex: Regex     = """Day(\d{2})\$?""".r
val yearRegex: Regex    = """aoc(\d{4})""".r
val answersRegex: Regex = """\d{2}:(\d+),(\d+)""".r

enum Part:
  case One, Two

abstract class Day:

  def day: Int = this.getClass.getSimpleName match
    case dayRegex(d) => d.toInt
    case _           => throw new IllegalArgumentException("Unable to extract day.")

  def year: Int = this.getClass.getPackageName match
    case yearRegex(y) => y.toInt
    case _            => throw new IllegalArgumentException("Unable to extract year.")

  def load(resource: String, preserveBlanks: Boolean = false): Array[String] =
    val source = Source.fromResource(resource)
    try {
      val xs = source.getLines.toArray
      if xs.size == 0 then throw new Exception("Input load failure: no lines.")
      // The lines iterator swallows the final blank line. Add it back in if desired.
      // Helpful when the input is a series of blank-line-separated groups.
      if preserveBlanks then xs :+ "" else xs
    } finally source.close()

  def loadInput(preserveBlanks: Boolean = false): Array[String] =
    val name = f"aoc$year%04d/Day$day%02d.input.txt"
    load(name, preserveBlanks)

  lazy val answers = load(f"aoc$year%04d/Answers.txt")
    .find(_.startsWith(f"$day%02d:"))
    .flatMap { case answersRegex(ans1, ans2) =>
      Some((ans1.toLong, ans2.toLong))
    }

  def solve(part: Part, solution: () => Long): Long =
    val t0   = System.currentTimeMillis()
    val soln = solution()
    val t1   = System.currentTimeMillis()

    val ans = (answers, part) match
      case (None, _)                => None
      case (Some((x, _)), Part.One) => Some(x)
      case (Some((_, x)), Part.Two) => Some(x)

    if ans.isDefined then assert(soln == ans.get)

    val ansChecked = ans.map(_ => "✔").getOrElse("➖")
    println(s"$ansChecked $soln [${t1 - t0} ms]")
    soln

  def solveP1(solution: () => Long): Long = solve(Part.One, solution)

  def solveP2(solution: () => Long): Long = solve(Part.Two, solution)

  def main(args: Array[String]): Unit
