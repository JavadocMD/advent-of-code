package aoc2016

import scala.annotation.tailrec
import aoc.Day

object Day20 extends Day:

  case class LongRange(a: Long, b: Long) extends Ordered[LongRange]:
    def compare(that: LongRange): Int = this.a.compare(that.a)
    def contains(n: Long): Boolean    = a <= n && n <= b
    def overlaps(that: LongRange): Boolean =
      this.contains(that.a) || this.contains(that.b)
        || that.contains(this.a) || that.contains(this.b)
        || this.a + 1 == that.b
    def size: Long = 1 + b - a

  lazy val input = loadInput()
    .map:
      case s"$a-$b" => LongRange(a.toLong, b.toLong)
    .sorted

  @tailrec
  def findLeast(curr: Long): Long =
    input.find(_.contains(curr)) match
      case Some(r) => findLeast(r.b + 1)
      case None    => curr

  lazy val part1 = findLeast(0)

  @tailrec
  def combine(candidates: List[LongRange], acc: List[LongRange] = Nil): List[LongRange] =
    candidates match
      case Nil => acc
      case head :: tail =>
        val (overlap, rest) = tail.partition(head.overlaps(_))
        if overlap.isEmpty then combine(tail, head :: acc)
        else
          val toCombine = head :: overlap
          val a         = toCombine.minBy(_.a).a
          val b         = toCombine.maxBy(_.b).b
          val combined  = LongRange(a, b)
          combine(combined :: rest, acc)

  lazy val part2 =
    val ranges = combine(input)
    val max    = LongRange(0, 4294967295L)
    max.size - ranges.map(_.size).sum

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
