package aoc2017

import scala.annotation.tailrec
import aoc.Day

object Day06 extends Day:

  lazy val input = loadInput().head.split("\\s+").toIndexedSeq.map(_.toInt)

  def redistribute(banks: IndexedSeq[Int]): IndexedSeq[Int] =
    val n          = banks.size
    val max        = banks.max
    val iMax       = banks.indexOf(max)
    val (div, rem) = (max / n, max % n)
    banks.zipWithIndex.map: (value, i) =>
      val j = (i - iMax + n - 1) % n
      val v = if i == iMax then 0 else value
      v + div + (if j < rem then 1 else 0)

  def cycles(initial: IndexedSeq[Int]): Iterator[IndexedSeq[Int]] =
    import scala.collection.{mutable as m}
    val memory = m.Set(initial)
    Iterator.unfold(initial): prev =>
      val next = redistribute(prev)
      if memory.contains(next) then None
      else
        memory.add(next)
        Some((next, next))

  lazy val part1 =
    // iterator doesn't include first or final state,
    // so cycles == size + 1
    cycles(input).size + 1

  extension [T](it: Iterator[T]) //
    def lastOption: Option[T] =
      var curr = Option.empty[T]
      while it.hasNext do curr = Some(it.next)
      curr

  lazy val part2 =
    val repeated = redistribute(cycles(input).lastOption.get)
    cycles(repeated).size + 1

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
