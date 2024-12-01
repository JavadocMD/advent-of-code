package aoc2019

import aoc._
import scala.annotation.tailrec

import aoc.Day

object Day16 extends Day:
  type Input = LazyList[Int]
  def parse(xs: Array[String]): Input = xs.head.to(LazyList).map(_.toString.toInt)

  class DigitCalculator(val i: Int):
    import DigitCalculator.State

    private[this] var state: State = if (i == 0) State.Add else State.Drop1
    private[this] var counter: Int = if (i == 0) 0 else i - 1
    private[this] var value: Int   = 0

    def calc(x: Int): Unit = {
      state match {
        case State.Add => value += x
        case State.Sub => value -= x
        case _         => // drop
      }
      if (counter == 0) {
        // Next state.
        counter = i
        state = State.next(state)
      } else {
        counter -= 1
      }
    }

    def get: Int = Math.abs(value % 10)

  object DigitCalculator:
    sealed trait State
    object State {
      case object Drop1 extends State
      case object Add   extends State
      case object Drop2 extends State
      case object Sub   extends State

      def next(state: State): State = state match {
        case Drop1 => Add
        case Add   => Drop2
        case Drop2 => Sub
        case Sub   => Drop1
      }
    }

  def digit(input: LazyList[Int], i: Int): Int =
    val x = new DigitCalculator(i)
    input.foreach(x.calc)
    x.get

  def phase(input: LazyList[Int]): LazyList[Int] = for {
    i <- (0 until input.length).to(LazyList)
  } yield digit(input, i)

  @tailrec
  final def phaseRepeat(input: LazyList[Int], times: Int): LazyList[Int] =
    if (times <= 0) input
    else phaseRepeat(phase(input), times - 1)

  def repeat(xs: LazyList[Int]): LazyList[Int] = xs #::: repeat(xs)

  def readOffset(input: LazyList[Int]): Int = input.take(7).mkString.toInt

  // Fast trick solver for part 2.
  def process(input: LazyList[Int]): LazyList[Int] = {
    val offset = readOffset(input)
    if (offset < (input.length * 5000)) {
      throw new Exception("Oops, requested offset isn't in last half of result.")
    }

    // the number of digits we need to track
    val n = (input.length * 10000) - offset
    // fill the signal backwards then flip it
    val signal = repeat(input.reverse).iterator
    var result = Array.fill(n)(signal.next).reverse
    var curr   = new Array[Int](n)

    def phase(): Unit = {
      var x = 0
      var i = n - 1
      while (i >= 0) {
        x = (x + result(i)) % 10
        curr(i) = x
        i -= 1
      }
      // Swap arrays.
      val temp = curr
      curr = result
      result = temp
    }

    for { i <- 0 until 100 } {
      phase()
    }

    LazyList.from(result.take(8))
  }

  // P1 solution uses a state machine which encodes the pattern and can calculate each digit in the result.
  def part1(input: Input): String = phaseRepeat(input, 100).take(8).mkString

  // P2 solution uses the trick that all offsets being requested are in the second half of the signal,
  // where the only contributing pattern is a series of 1's from that digit to the end of the signal.
  // The value of a digit is the sum of the last phase's digit (at this index) and
  // the digit in this phase to the right (mod 10). The right-most digit never changes from its input value.
  // We can save time by only calculating the back half of each phase (stopping at our offset index).
  def part2(input: Input): String = process(input).mkString

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
