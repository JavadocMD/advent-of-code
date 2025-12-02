package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day06MultiSet extends Day:
  type Input = String
  def parse(xs: String): Input = xs

  def part1(input: Input) = Scanner.findMarker(input, length = 4).get._2

  def part2(input: Input) = Scanner.findMarker(input, length = 14).get._2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().head)
    part1(in) // warm up cache
    solveP1(() => part1(in))
    solveP2(() => part2(in))

  object Scanner:

    object MultiSet:
      def apply(xs: IterableOnce[Char]): MultiSet =
        val set = new MultiSet()
        xs.foreach(set.add)
        set
    end MultiSet

    class MultiSet:
      private val counts = new Array[Int](128)
      private var total  = 0
      def size           = total

      def add(c: Char) =
        val count = counts(c.toInt)
        if count == 0 then total += 1
        counts(c.toInt) += 1

      def remove(c: Char) =
        val count = counts(c.toInt)
        if count > 0 then
          if count == 1 then total -= 1
          counts(c.toInt) -= 1
    end MultiSet

    def findMarker(signal: String, length: Int): Option[(Int, Int)] =
      val counts = MultiSet(signal.view.take(length))
      @tailrec
      def loop(previous: Char, i: Int, j: Int): Option[(Int, Int)] =
        if counts.size == length then Some((i, i + length)) // found the index
        else if j >= signal.length then None                // no valid answer!
        else
          val next = signal(i + 1)
          val last = signal(j)
          if previous != last then
            counts.remove(previous)
            counts.add(last)
          loop(next, i = i + 1, j = j + 1)
      end loop
      loop(signal(0), i = 0, j = length)

    def findMarkerEnd(signal: String, length: Int): Int =
      findMarker(signal, length) match
        case Some((_, n)) => n
        case None         => -1

  end Scanner
