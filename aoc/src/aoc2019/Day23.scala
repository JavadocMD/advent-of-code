package aoc2019

import aoc._
import scala.annotation.tailrec

import aoc.Day

object Day23 extends Day:
  import IntcodeComputer as C
  import scala.collection.mutable.Queue

  type Input = C.Program
  def parse(xs: Array[String]): Input = C.parseMemory(xs.head)

  def mul3(n: Int) = n - (n % 3)

  class Nic(val address: Long, program: C.Program):
    private[this] var input = Queue(address)
    private[this] val inputIterator = new Iterator[Long]() {
      override def hasNext: Boolean = true
      override def next(): Long = {
        if (input.nonEmpty) input.dequeue() else -1L
      }
    }

    private[this] var state = C.State(program, inputIterator)

    def isEmpty: Boolean = input.isEmpty

    def enqueue(x: Long, y: Long): Unit =
      input.enqueue(x)
      input.enqueue(y)

    def step(): Iterator[(Long, Long, Long)] =
      state = IntcodeComputer.step(state)
      val n = state.output.length
      if (n < 3) Iterator.empty
      else {
        val (out, rem) = state.output.splitAt(mul3(n))
        state = state.copy(output = rem)
        out.grouped(3).map({ case Seq(a, x, y) => (a, x, y) })
      }
  end Nic

  def part1(program: C.Program): Long =
    val nics = for {
      address <- (0 until 50).toIndexedSeq
    } yield new Nic(address, program)

    var result: Option[Long] = None
    while (result.isEmpty) {
      for {
        out             <- nics.map(_.step())
        (address, x, y) <- out
      } {
        if (address == 255L && result == None) {
          result = Some(y)
        } else {
          nics(address.toInt).enqueue(x, y)
        }
      }
    }
    result.get
  end part1

  def part2(program: C.Program): Long =
    val nics = for {
      address <- (0 until 50).toIndexedSeq
    } yield new Nic(address, program)

    var natX = -1L
    var natY = -1L

    var idle = 0

    var natYSent0            = Long.MinValue
    var result: Option[Long] = None
    while (result.isEmpty) {
      var sent = 0 // count packets sent this cycle
      for {
        out             <- nics.map(_.step())
        (address, x, y) <- out
      } {
        sent += 1
        if (address == 255L) {
          natX = x
          natY = y
        } else {
          nics(address.toInt).enqueue(x, y)
        }
      }

      // Detect idle...
      if (nics.forall(_.isEmpty) && sent == 0) {
        idle += 1
      }

      // If idle for # cycles trigger NAT send to 0.
      if (idle >= 750) {
        idle = 0
        nics(0).enqueue(natX, natY)
        // Detect termination condition.
        if (natY == natYSent0) {
          result = Some(natY)
        } else {
          natYSent0 = natY
        }
      }
    }
    result.get
  end part2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
