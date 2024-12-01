package aoc2019

import aoc._
import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day09 extends Day:
  import IntcodeComputer as C

  type Input = C.Program
  def parse(xs: Array[String]): Input = C.parseMemory(xs.head)

  def part1(input: Input): Long =
    val state0 = C.State(input, Seq(1L).iterator)
    val res0   = C.run(state0)
    res0.output.head

  def part2(input: Input): Long =
    val state1 = C.State(input, Seq(2L).iterator)
    val res1   = C.run(state1)
    res1.output.head

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
