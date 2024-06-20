package aoc2020

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.{mutable => m}

import aoc.Day

object Day14 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  // Parse a string as a binary value where only chars matching `c` are 1
  def only(c: Char)(s: String): Long =
    s.foldLeft(0L)({
      case (acc, `c`) => (acc << 1) | 1
      case (acc, _)   => (acc << 1)
    })

  val MaskOp = "^mask = ([01X]+)$".r
  val MemOp  = "^mem\\[([0-9]+)\\] = ([0-9]+)$".r

  class BitmaskComputer:
    private[this] var zeroMask = 0L // 1 where mask is 0
    private[this] var onesMask = 0L // 1 where mask is 1
    private[this] val mem      = m.Map.empty[Long, Long]

    def setMask(m: String): Unit = {
      zeroMask = only('0')(m)
      onesMask = only('1')(m)
    }

    def setMem(address: Long, value: Long): Unit = {
      // force 0 bits => invert, `or` the zero mask, invert again
      // force 1 bits => `or` the ones mask
      val actual = ~(~value | zeroMask) | onesMask
      mem(address) = actual
    }

    def getMem: Iterable[Long] = mem.values

  def part1(input: Input): Long =
    val computer = new BitmaskComputer()
    input.foreach({
      case MaskOp(m)   => computer.setMask(m)
      case MemOp(a, v) => computer.setMem(a.toLong, v.toLong)
      case s           => throw new Exception(s"Unknown op: $s")
    })
    computer.getMem.sum

  class BitmaskComputer2:
    private[this] var onesMask = 0L
    private[this] var zeroMask = 0L
    private[this] var offsets  = List.empty[Long]
    private[this] val mem      = m.Map.empty[Long, Long]

    def setMask(m: String): Unit = {
      onesMask = only('1')(m)
      zeroMask = only('X')(m) // we will need to zero out all floating bits
      // get the numeric value of every 'X'
      val floaters = for {
        (x, i) <- m.reverse.zipWithIndex if x == 'X'
      } yield 1L << i
      // and calc every offset from the base address by both adding and not adding each value
      offsets = floaters.foldLeft(List(0L)) { case (xs, value) =>
        xs ++ xs.map(_ + value)
      }
    }

    def setMem(address: Long, value: Long): Unit = {
      val base = ~(~address | zeroMask) | onesMask
      offsets.foreach(f => {
        mem(base + f) = value
      })
    }

    def getMem: Iterable[Long] = mem.values

  def part2(input: Input): Long =
    val computer = new BitmaskComputer2()
    input.foreach({
      case MaskOp(m)   => computer.setMask(m)
      case MemOp(a, v) => computer.setMem(a.toLong, v.toLong)
      case s           => throw new Exception(s"Unknown op: $s")
    })
    computer.getMem.sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
