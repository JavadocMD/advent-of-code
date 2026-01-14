package aoc2016

import scala.annotation.tailrec
import aoc.Day

object Day19 extends Day:

  lazy val input = loadInput().head.toInt

  lazy val part1 =
    @tailrec
    def recurse(nums: List[Int]): Int =
      val trailing = nums.size % 2 == 1
      val next = nums.view.zipWithIndex
        .filter((n, i) => i % 2 == 0)
        .map((n, i) => n)
        .toList
      if next.size == 1 then next.head
      else recurse(if trailing then next.tail else next)

    recurse(List.from(1 to input))

  class LinkedList(val value: Int):
    var next: LinkedList = null

    def advance(n: Int): LinkedList =
      var curr = this
      for _ <- 0 until n do curr = curr.next
      curr

    def deleteNext(): Unit =
      this.next = this.next.next

  object LinkedList:
    def circular(first: Int, last: Int): LinkedList =
      val head = LinkedList(first)
      var prev = head
      for i <- first + 1 to last do
        prev.next = LinkedList(i)
        prev = prev.next
      prev.next = head
      head

  lazy val part2 =
    var n    = input
    var curr = LinkedList.circular(1, n)

    // Track the "removal" point: each round we remove the node following this one.
    var removal = curr.advance(n / 2 - 1)

    while n > 1 do
      // Drop the target elf!
      removal.deleteNext()
      // The removal point only advances if we had an odd number of elves this round.
      if n % 2 == 1 then removal = removal.next
      // Next elf's turn, there's one less elf.
      curr = curr.next
      n -= 1

    curr.value

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
