package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day06Loops extends Day:
  type Input = String
  def parse(xs: String): Input = xs

  def part1(input: Input) = findMarker(input, length = 4)

  def part2(input: Input) = findMarker(input, length = 14)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().head)
    part1(in) // warm up cache
    solveP1(() => part1(in))
    solveP2(() => part2(in))

  @tailrec
  def findMarker(signal: String, length: Int, position: Int = 0): Int =
    @tailrec
    def findDuplicate(check: Int, cursor: Int, stop: Int): Int =
      // when check advances to stop, conclude no duplicate found
      if check == stop then -1
      // when cursor advances to stop, advance check and reset cursor
      else if cursor == stop then findDuplicate(check + 1, check + 2, stop)
      // if the characters at check and cursor are equal, duplicate found
      // return the index at which to continue the outer search
      else if signal(check) == signal(cursor) then check + 1
      // otherwise, advance cursor
      else findDuplicate(check, cursor + 1, stop)

    if position + length > signal.length then -1 // stop if there aren't enough characters left to make the marker
    else
      // search for duplicates within `length` of our current position
      findDuplicate(position, position + 1, position + length) match
        case -1   => position + length                           // no duplicate means we've found the marker! success
        case next => findMarker(signal, length, position = next) // otherwise keep looking
  end findMarker
