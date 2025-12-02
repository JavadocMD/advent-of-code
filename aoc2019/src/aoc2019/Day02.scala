package aoc2019

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day02 extends Day:
  type Input = IndexedSeq[Int]
  def parse(xs: Array[String]): Input = xs.head.split(",").map(_.toInt).toIndexedSeq

  def binaryInstruction(memory: IndexedSeq[Int], pointer: Int, operator: (Int, Int) => Int): IndexedSeq[Int] =
    val iA = memory(pointer + 1)
    val iB = memory(pointer + 2)
    val iR = memory(pointer + 3)
    memory.updated(iR, operator(memory(iA), memory(iB)))

  def add(a: Int, b: Int): Int      = a + b
  def multiply(a: Int, b: Int): Int = a * b

  @tailrec
  final def runProgram(memory: IndexedSeq[Int], pointer: Int = 0): IndexedSeq[Int] =
    memory(pointer) match {
      case 1 =>
        val nextState = binaryInstruction(memory, pointer, add)
        runProgram(nextState, pointer + 4)
      case 2 =>
        val nextState = binaryInstruction(memory, pointer, multiply)
        runProgram(nextState, pointer + 4)
      case 99 => memory
      case x  => throw new Exception(s"Encountered unknown opcode $x at index $pointer")
    }

  def programSearch(stopResult: Int, initialMemory: IndexedSeq[Int]): Option[(Int, Int)] =
    val combos = for {
      noun <- LazyList.range(0, 100)
      verb <- LazyList.range(0, 100)
    } yield (noun, verb)

    combos.find { case (noun, verb) =>
      val prg = initialMemory.updated(1, noun).updated(2, verb)
      val res = runProgram(prg)
      res(0) == stopResult
    }

  def part1(input: Input): Long =
    runProgram(input.updated(1, 12).updated(2, 2))(0)

  def part2(input: Input): Long =
    programSearch(19690720, input) match
      case Some((noun, verb)) => 100 * noun + verb
      case None               => throw Exception("Search produced no viable result.")

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
