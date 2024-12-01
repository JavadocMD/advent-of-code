package aoc2019

import aoc._
import scala.annotation.tailrec

import aoc.Day

object Day21 extends Day:
  import IntcodeComputer as C

  type Input = C.Program
  def parse(xs: Array[String]): Input = C.parseMemory(xs.head)

  object Ascii:
    def encode(s: String): Seq[Long] = s.map(_.toLong)

    def decode(xs: Seq[Long]): String =
      val b = new StringBuilder()
      xs.foreach {
        case c if c < 128 => b.addOne(c.toChar)
        case n            => b.addAll(n.toString)
      }
      b.result()

  def part1(program: Input): String =
    // J = !(A && B && C) && D
    val code = """
      |NOT J J
      |AND A J
      |AND B J
      |AND C J
      |NOT J J
      |AND D J
      |WALK
      |""".stripMargin.stripPrefix("\n")
    // Note: program `J = D` gets us in tough spots

    val s0     = IntcodeComputer.State(program, Ascii.encode(code).iterator)
    val sf     = IntcodeComputer.run(s0)
    val result = Ascii.decode(sf.output)
    result.linesIterator.toList.last

  def part2(program: Input): String =
    // While running, we still only jump four spaces,
    // however there are situations where we can make a safe initial jump
    // but upon landing have no safe jump.
    // i.e.: ###.#.##.##.###
    // Jumping from the first position lands successfully, but would require an
    // immediate jump when four spaces from there is a hole.

    // This solution was a bit of a guess actually...
    // J = !(A && B && C) && D && (E || H)
    val code = """
      |NOT J J
      |AND A J
      |AND B J
      |AND C J
      |NOT J J
      |AND D J
      |OR E T
      |OR H T
      |AND T J
      |RUN
      |""".stripMargin.stripPrefix("\n")

    val s0     = IntcodeComputer.State(program, Ascii.encode(code).iterator)
    val sf     = IntcodeComputer.run(s0)
    val result = Ascii.decode(sf.output)
    result.linesIterator.toList.last

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
