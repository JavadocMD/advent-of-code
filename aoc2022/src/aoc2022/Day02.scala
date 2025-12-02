package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day02 extends Day:
  type Glyph1 = 'A' | 'B' | 'C'
  type Glyph2 = 'X' | 'Y' | 'Z'
  val rowPattern = """(A|B|C) (X|Y|Z)""".r

  type Input = List[(Glyph1, Glyph2)]
  def parse(xs: Array[String]): Input = xs.toList.map {
    case rowPattern(m1, m2) =>
      val g0: Glyph1 = m1.charAt(0).asInstanceOf[Glyph1]
      val g1: Glyph2 = m2.charAt(0).asInstanceOf[Glyph2]
      (g0, g1)
    case x =>
      throw new Exception(s"Input parse error: unexpected row format (${x})")
  }

  enum Shape(val score: Int):
    case Rock     extends Shape(1)
    case Paper    extends Shape(2)
    case Scissors extends Shape(3)
  end Shape
  import Shape._

  object Shape:
    def parse(x: Glyph1 | Glyph2): Shape = x match
      case 'A' | 'X' => Rock
      case 'B' | 'Y' => Paper
      case 'C' | 'Z' => Scissors
  end Shape

  enum Outcome(val score: Int):
    case Win  extends Outcome(6)
    case Draw extends Outcome(3)
    case Lose extends Outcome(0)
  end Outcome
  import Outcome._

  object Outcome:
    def parse(x: Glyph2): Outcome = x match
      case 'X' => Lose
      case 'Y' => Draw
      case 'Z' => Win

    // what is the outcome for the chosen shapes?
    def determine(theirs: Shape, mine: Shape): Outcome = (theirs, mine) match
      case (Rock, Paper)     => Win
      case (Rock, Scissors)  => Lose
      case (Paper, Rock)     => Lose
      case (Paper, Scissors) => Win
      case (Scissors, Rock)  => Win
      case (Scissors, Paper) => Lose
      case _                 => Draw

    // which shape yields the desired outcome against their choice?
    def against(desired: Outcome, theirs: Shape): Shape = (desired, theirs) match
      case (Win, Rock)      => Paper
      case (Win, Paper)     => Scissors
      case (Win, Scissors)  => Rock
      case (Lose, Rock)     => Scissors
      case (Lose, Paper)    => Rock
      case (Lose, Scissors) => Paper
      case (Draw, s)        => s
  end Outcome

  def part1(input: Input) = input.map { (s0: Glyph1, s1: Glyph2) =>
    val theirs  = Shape.parse(s0)
    val mine    = Shape.parse(s1)
    val outcome = Outcome.determine(theirs, mine)
    outcome.score + mine.score
  }.sum

  def part2(input: Input) = input.map { (s0: Glyph1, s1: Glyph2) =>
    val theirs  = Shape.parse(s0)
    val outcome = Outcome.parse(s1)
    val mine    = Outcome.against(outcome, theirs)
    outcome.score + mine.score
  }.sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
