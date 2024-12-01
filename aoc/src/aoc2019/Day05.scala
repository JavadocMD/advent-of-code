package aoc2019

import aoc.Day
import fastparse.*

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Failure
import scala.util.Success

object Day05 extends Day:
  type Input = IndexedSeq[Int]
  def parse(xs: Array[String]): Input = xs.head
    .split(",")
    .map(_.toInt)
    .toIndexedSeq

  implicit val exec: ExecutionContextExecutor = ExecutionContext.global

  def part1(prg0: Input): Int =
    var res0 = Vector[Int]()
    val cmp0 = new IntcodeComputer(() => Future.successful(1), n => res0 :+= n)
    val fut0 = cmp0.runProgram(prg0) map { _ => res0.last }
    Await.result(fut0, Duration.Inf)

  def part2(prg1: Input): Int =
    var res1 = Vector[Int]()
    val cmp1 = new IntcodeComputer(() => Future.successful(5), n => res1 :+= n)
    val fut1 = cmp1.runProgram(prg1) map { _ => res1.last }
    Await.result(fut1, Duration.Inf)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))

  class IntcodeComputer(input: () => Future[Int], output: (Int) => Unit)(implicit exec: ExecutionContext):
    type Mem     = IndexedSeq[Int]
    type Pointer = Int
    type State   = (Mem, Pointer)

    case class OpCode(id: Int, mode: Int)
    object OpCode {
      def parse(x: Int): OpCode = OpCode(x % 100, x / 100)
    }

    type Op = (OpCode, State) => Future[State]

    // Primitive Operations

    def readData(state: State, offset: Int, mode: Int): Int = {
      val (mem, pointer) = state
      val x              = mem(pointer + offset)
      val immediate      = ((mode >>> (offset - 1)) & 1) == 1
      if (immediate) x else mem(x)
    }

    def readRegister(state: State, offset: Int): Int = {
      val (mem, pointer) = state
      mem(pointer + offset)
    }

    // Coded Operations

    def inputOp(op: OpCode, state: State): Future[State] = {
      val (mem, pointer) = state
      val iR             = readRegister(state, 1)
      for { value <- input() } yield (mem.updated(iR, value), pointer + 2)
    }

    def outputOp(op: OpCode, state: State): Future[State] = {
      val (mem, pointer) = state
      val value          = readData(state, 1, op.mode)
      output(value)
      Future.successful((mem, pointer + 2))
    }

    def jumpOp(predicate: (Int) => Boolean)(op: OpCode, state: State): Future[State] = {
      val (mem, pointer) = state
      val a              = readData(state, 1, op.mode)
      val j              = readData(state, 2, op.mode)
      val nextPointer    = if (predicate(a)) j else pointer + 3
      Future.successful((mem, nextPointer))
    }
    val jumpIfTrue  = jumpOp(_ != 0) _
    val jumpIfFalse = jumpOp(_ == 0) _

    def testOp(comparison: (Int, Int) => Boolean)(op: OpCode, state: State): Future[State] = {
      val (mem, pointer) = state
      val a              = readData(state, 1, op.mode)
      val b              = readData(state, 2, op.mode)
      val iR             = readRegister(state, 3)
      val value          = if (comparison(a, b)) 1 else 0
      Future.successful((mem.updated(iR, value), pointer + 4))
    }
    val lessThan = testOp(_ < _) _
    val equals   = testOp(_ == _) _

    def mathOp(operator: (Int, Int) => Int)(op: OpCode, state: State): Future[State] = {
      val (mem, pointer) = state
      val a              = readData(state, 1, op.mode)
      val b              = readData(state, 2, op.mode)
      val iR             = readRegister(state, 3)
      val value          = operator(a, b)
      Future.successful((mem.updated(iR, value), pointer + 4))
    }
    val add      = mathOp(_ + _) _
    val multiply = mathOp(_ * _) _

    val getOp: PartialFunction[OpCode, Op] = op =>
      op.id match {
        case 1 => add
        case 2 => multiply
        case 3 => inputOp
        case 4 => outputOp
        case 5 => jumpIfTrue
        case 6 => jumpIfFalse
        case 7 => lessThan
        case 8 => equals
      }

    // Execution

    final def runProgram(memory: Mem, pointer: Int = 0): Future[Mem] = {
      val opcode = OpCode.parse(memory(pointer))
      // println(opcode.toString + ':' + mode.toString)
      opcode match {
        // Normal op: process step.
        case x if getOp.isDefinedAt(x) =>
          val state = (memory, pointer)
          val next  = getOp(x)(opcode, state)
          next.flatMap({ case (m, p) => runProgram(m, p) })
        // Termination: return final memory.
        case OpCode(99, _) => Future.successful(memory)
        // Error: unknown opcode.
        case _ => Future.failed[Mem](new Exception(s"Encountered unknown $opcode at index $pointer"))
      }
    }
