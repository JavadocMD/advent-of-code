package aoc2019

import aoc._
import scala.annotation.tailrec

import aoc.Day

object Day13 extends Day:
  import IntcodeComputer as C

  type Input = C.Program
  def parse(xs: Array[String]): Input = C.parseMemory(xs.head)

  def countBlocks(prg: IndexedSeq[Long]): Int =
    import C.{State, step}

    var blocks = 0

    /** Run the program until complete or we have three outputs.
      */
    @tailrec
    def run(s: State): State =
      if (s.isComplete) s
      else if (s.output.length == 3) s
      else run(step(s))

    /** Run the game until complete.
      */
    @tailrec
    def cycle(s0: State): State = {
      run(s0) match {
        case s if s.isComplete => s0
        case s @ State(_, _, x :: y :: tile :: Nil, _, _) =>
          if (tile == 2) {
            blocks += 1
          }
          cycle(s.copy(output = Seq()))
        case x => throw new Exception(s"Unexpected program state: $x")
      }
    }

    cycle(State(prg, Iterator.empty))
    blocks
  end countBlocks

  def trackScore(prg: IndexedSeq[Long]): Long =
    import C.{State, step}

    var score   = 0L
    var paddleX = 0L
    var ballX   = 0L

    // Inputs
    val left    = Iterator.continually(-1L)
    val neutral = Iterator.continually(0L)
    val right   = Iterator.continually(1L)

    /** Run the program until complete or we have three outputs.
      */
    @tailrec
    def run(s: State): State =
      if (s.isComplete) s
      else if (s.output.length == 3) s
      else run(step(s))

    /** Run the game until complete.
      */
    @tailrec
    def cycle(s0: State): State = run(s0) match {
      case s if s.isComplete => s0
      case s @ State(_, _, x :: y :: tile :: Nil, _, _) =>
        if (x == -1) {
          score = tile
        } else if (tile == 3) {
          paddleX = x
        } else if (tile == 4) {
          ballX = x
        }
        val input = if (ballX < paddleX) left else if (ballX > paddleX) right else neutral
        cycle(s.copy(output = Seq(), input = input))
      case x => throw new Exception(s"Unexpected program state: $x")
    }

    cycle(State(prg, neutral))
    score

  end trackScore

  def part1(input: Input): Long = countBlocks(input)

  def part2(input: Input): Long =
    val prg1 = input.updated(0, 2L) // Set mem:0 to 2 (quarters)
    trackScore(prg1)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
