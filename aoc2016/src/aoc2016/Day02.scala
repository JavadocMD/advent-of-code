package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Util._

object Day02 extends Day:

  lazy val input: List[List[Char]] = loadInput().map(_.toList)

  class Keypad(layout: String):
    val buttons = Map.from(
      for
        (line, j) <- layout.linesIterator.zipWithIndex
        (char, i) <- line.zipWithIndex
        if char != ' '
      yield Vector2(i, j) -> char
    )

    def simulateOne(start: Vector2, moves: List[Char]): Vector2 =
      moves.foldLeft(start): (prev, m) =>
        val next = m match
          case 'R' => prev + E.vector
          case 'D' => prev + S.vector
          case 'L' => prev + W.vector
          case 'U' => prev + N.vector
        if buttons.contains(next) then next else prev

    def simulateAll(moves: List[List[Char]]): String =
      val start = buttons.find(_._2 == '5').get._1
      moves
        .scanLeft(start)(simulateOne)
        .map(buttons)
        .tail
        .mkString

  lazy val part1 = Keypad("123\n456\n789").simulateAll(input)

  lazy val part2 = Keypad("  1  \n 234 \n56789\n ABC \n  D  ").simulateAll(input)

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
