package aoc2022

import scala.annotation.tailrec
import scala.collection.{mutable => m}

import aoc.Day

object Day20 extends Day:
  class Number(val value: Long):
    override def toString = value.toString

  type Input = List[Number]
  def parse(xs: Iterator[String]): Input =
    xs.map(s => new Number(s.toLong)).toList

  extension [T](xs: List[T])
    def remove(i: Int): List[T] =
      val (head, tail) = xs.splitAt(i)
      head ::: tail.tail
    def insert(i: Int, value: T): List[T] =
      val (head, tail) = xs.splitAt(i)
      head ::: (value :: tail)

  def insertIndex(number: Long, remIndex: Int, size: Int): Int =
    if number == 0 then remIndex
    else
      var insIndex = ((remIndex + number) % (size - 1)).toInt
      while insIndex < 0 do insIndex += size - 1
      insIndex

  def mix(curr: List[Number], number: Number): List[Number] =
    val remIndex = curr.indexOf(number)
    val insIndex = insertIndex(number.value, remIndex, curr.size)
    curr
      .remove(remIndex)
      .insert(insIndex, number)

  def mixAll(orig: List[Number], list: List[Number]): List[Number] = orig.foldLeft(list) { mix(_, _) }

  def part1(input: Input) =
    val xs          = mixAll(input, input)
    val zeroIndex   = xs.indexWhere(_.value == 0)
    def nth(n: Int) = xs((zeroIndex + n) % xs.size).value
    nth(1000) + nth(2000) + nth(3000)

  def part2(input: Input) =
    val decKey      = 811589153L
    val list        = input.map { case n => new Number(n.value * decKey) }
    val xs          = (0 until 10).foldLeft(list) { (xs, _) => mixAll(list, xs) }
    val zeroIndex   = xs.indexWhere(_.value == 0)
    def nth(n: Int) = xs((zeroIndex + n) % xs.size).value
    nth(1000) + nth(2000) + nth(3000)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
