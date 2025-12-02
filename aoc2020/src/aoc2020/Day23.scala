package aoc2020

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder

import aoc.Day

object Day23 extends Day:
  type Input = Seq[Int]
  def parse(xs: Array[String]): Input = xs.head.map(_.toString.toInt)

  // `size` is the id of the largest cup.
  class CupRing(size: Int, initialOrder: Seq[Int]):
    // For each cup (index = id - 1), store the _index_ of its clockwise neighbor.
    // Note we have to be careful to convert between index and id representations.
    val nextArray                           = Array.range(1, size + 1)
    def setNext(id: Int, nextId: Int): Unit = { nextArray(id - 1) = (nextId - 1) }
    def next(id: Int): Int                  = nextArray(id - 1) + 1

    // Apply initial ordering.
    pairsIterator(initialOrder).foreach({ case (currId, nextId) =>
      setNext(currId, nextId)
    })
    // And fix boundary references for cases where the given ordering
    // is total (part 1) or implied (part 2).
    if (size == initialOrder.size) {
      setNext(initialOrder.last, initialOrder.head)
    } else {
      setNext(initialOrder.last, initialOrder.max + 1)
      setNext(size, initialOrder.head)
    }

    // Returns `n` cups clockwise of 1 (not including 1).
    def toSeq(n: Int): Seq[Int] =
      var res  = Seq.empty[Int]
      var curr = next(1)
      for (i <- 0 until n) {
        res = res.appended(curr)
        curr = next(curr)
      }
      res

  end CupRing

  def play(size: Int, cups: Seq[Int], turns: Int): CupRing =
    val ring = new CupRing(size, cups)
    import ring.{next, setNext}

    // Decrement, wrapping around to `size` instead of 0.
    def dec(n: Int): Int = if (n == 1) size else n - 1

    var curr = cups.head
    for (t <- 0 until turns) {
      // Pick the three clockwise cups.
      val pick1 = next(curr)
      val pick2 = next(pick1)
      val pick3 = next(pick2)
      // Find destination.
      var dest = dec(curr)
      while (dest == pick1 || dest == pick2 || dest == pick3) {
        dest = dec(dest)
      }
      // Move the three picked cups to destination.
      setNext(curr, next(pick3))
      setNext(pick3, next(dest))
      setNext(dest, pick1)
      // Advance current cup.
      curr = next(curr)
    }

    ring
  end play

  def part1(cups: Input): String =
    val ring = play(cups.max, cups, 100)
    ring.toSeq(cups.size - 1).mkString

  def part2(cups: Input): Long =
    val ring = play(1_000_000, cups, 10_000_000)
    ring.toSeq(2).map(_.toLong).product

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
