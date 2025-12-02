package aoc2019

import aoc._
import scala.annotation.tailrec

import aoc.Day

object Day22 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  case class Deck(size: Int, cards: LazyList[Int]):
    def dins: Deck = copy(cards = cards.reverse)

    def cut(n: Int): Deck =
      if (n == 0) this
      else {
        val i      = if (n > 0) n else size + n
        val (a, b) = cards.splitAt(i)
        copy(cards = b ++ a)
      }

    def dinc(n: Int): Deck =
      val array = Array.ofDim[Int](size)
      var i     = 0
      cards.foreach { c =>
        array(i) = c
        i = (i + n) % size
      }
      copy(cards = LazyList.from(array))

  object Deck:
    def of(size: Int) = Deck(size, LazyList.range(0, size))

  sealed trait Op
  object Op:
    case object Dins        extends Op
    case class Cut(n: Int)  extends Op
    case class Dinc(n: Int) extends Op

    def parse(s: String): Op =
      if (s.startsWith("deal into new stack")) Dins
      else if (s.startsWith("cut ")) Cut(s.drop(4).toInt)
      else if (s.startsWith("deal with increment ")) Dinc(s.drop(20).toInt)
      else throw new Exception(s"Unknown shuffle: $s")

  def shuffle(size: Int, steps: Array[Op]): Deck =
    val d0 = Deck.of(size)
    steps.foldLeft(d0) {
      case (d, Op.Dins)    => d.dins
      case (d, Op.Cut(n))  => d.cut(n)
      case (d, Op.Dinc(n)) => d.dinc(n)
    }

  def part1(input: Input): Long =
    val inputOps = input.map(Op.parse)
    shuffle(10007, inputOps).cards.indexOf(2019)

  // Adapted from: https://todd.ginsberg.com/post/advent-of-code/2019/day22/

  val ZERO            = BigInt(0)
  val ONE             = BigInt(1)
  val TWO             = BigInt(2)
  val FIND            = BigInt(2020)
  val NUMBER_OF_CARDS = BigInt("119315717514047")
  val SHUFFLES        = BigInt("101741582076661")

  def getBigInteger(s: String): BigInt = BigInt(s.split(" ").last)

  def part2(input: Array[String]): BigInt =
    var (mem0, mem1) = (ONE, ZERO)

    input.reverseIterator.foreach { op =>
      if (op.contains("cut")) {
        mem1 += getBigInteger(op)
      } else if (op.contains("increment")) {
        val x = getBigInteger(op).modPow(NUMBER_OF_CARDS - TWO, NUMBER_OF_CARDS)
        mem0 *= x
        mem1 *= x
      } else if (op.contains("stack")) {
        mem0 = -mem0
        mem1 = -(mem1 + ONE)
      }
      mem0 %= NUMBER_OF_CARDS
      mem1 %= NUMBER_OF_CARDS
    }

    val power = mem0.modPow(SHUFFLES, NUMBER_OF_CARDS)
    val A     = power * FIND
    val B     = mem1 * (power + NUMBER_OF_CARDS - ONE)
    val C     = (mem0 - ONE).modPow(NUMBER_OF_CARDS - TWO, NUMBER_OF_CARDS)
    (A + B * C).mod(NUMBER_OF_CARDS)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
