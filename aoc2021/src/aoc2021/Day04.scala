package aoc2021

import scala.annotation.tailrec
import scala.collection.{mutable => m}

import aoc.Day

object Day04 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  type Board = List[m.Set[Int]]

  def part1(input: Input) =
    val numbers = input(0).split(",").map(_.toInt).iterator
    val boards: List[Board] = input.iterator
      .drop(2)
      .grouped(6)
      .map(xs => {
        val rows = xs
          .take(5)
          .map(_.trim.split("\\s+").toList.map(_.toInt))
          .toList
        val cols = rows.transpose
        (rows ++ cols).map(m.Set.from(_))
      })
      .toList

    def isWinner(board: Board): Boolean = board.exists(_.isEmpty)

    def mark(number: Int): Unit =
      for
        b  <- boards
        xs <- b
      do xs -= number

    var n = -1

    var winner: Option[Board] = None
    while winner.isEmpty && numbers.hasNext do
      n = numbers.next
      mark(n)
      winner = boards.find(isWinner(_))

    val sumUnmarked = winner.get.take(5).map(_.sum).sum
    sumUnmarked * n

  def part2(input: Input) =
    val numbers = input(0).split(",").map(_.toInt).iterator
    var boards: List[Board] = input.iterator
      .drop(2)
      .grouped(6)
      .map(xs => {
        val rows = xs
          .take(5)
          .map(_.trim.split("\\s+").toList.map(_.toInt))
          .toList
        val cols = rows.transpose
        (rows ++ cols).map(m.Set.from(_))
      })
      .toList

    def isWinner(board: Board): Boolean = board.exists(_.isEmpty)

    def mark(number: Int): Unit =
      for
        b  <- boards
        xs <- b
      do xs -= number

    var n = -1

    var last: Option[Board] = None
    while last.isEmpty && numbers.hasNext do
      n = numbers.next
      mark(n)
      boards = boards.filter(!isWinner(_))
      last = if boards.size == 1 then Some(boards.head) else None

    var winner: Option[Board] = None
    while winner.isEmpty && numbers.hasNext do
      n = numbers.next
      mark(n)
      winner = boards.find(isWinner(_))

    val sumUnmarked = winner.get.take(5).map(_.sum).sum
    sumUnmarked * n

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
