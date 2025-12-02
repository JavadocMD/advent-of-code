package aoc2019

import scala.annotation.tailrec

import aoc.Day

import aoc2019.{IntcodeComputer => Comp}

object Day07 extends Day:
  type Phases = Seq[Long]
  type Input  = Comp.Program
  def parse(xs: Array[String]): Input = xs.head.split(",").view.map(_.toLong).toIndexedSeq

  def runSequence(prg: Comp.Program, phases: Phases): Long =
    phases.foldLeft(0L):
      case (input, phase) =>
        val s0 = Comp.State(prg, input = Seq(phase, input).iterator)
        val s1 = Comp.run(s0)
        s1.output.last

  def findBestSequence(prg: Comp.Program): (Phases, Long) =
    Seq[Long](0, 1, 2, 3, 4).permutations
      .map(ps => (ps, runSequence(prg, ps)))
      .maxBy(_._2)

  def runFeedback(prg: Comp.Program, phases: Phases): Long =
    @tailrec
    def recurse(amps: List[Comp.State], firstInput: Long): Long =
      if amps.forall(_.isComplete) then firstInput
      else
        val (nextAmps, lastOutput) = amps.foldLeft((List.empty[Comp.State], firstInput)):
          case ((acc, input), curr) =>
            val newInputs = List.from(curr.input).appended(input)
            val next      = Comp.runIOSync1(curr, newInputs)
            (next :: acc, next.output.head)
        recurse(nextAmps.reverse, lastOutput)

    val amps = phases.map(p => Comp.State(prg, Seq(p).iterator)).toList
    recurse(amps, 0L)

  def findBestFeedback(prg: Comp.Program): (Phases, Long) =
    Seq[Long](5, 6, 7, 8, 9).permutations
      .map(ps => (ps, runFeedback(prg, ps)))
      .maxBy(_._2)

  def part1(input: Input): Long = findBestSequence(input)._2

  def part2(input: Input): Long = findBestFeedback(input)._2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
