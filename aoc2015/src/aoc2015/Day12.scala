package aoc2015

import scala.annotation.tailrec
import aoc.Day
import aoc2015.Util._
import ujson.Obj
import ujson.Arr

object Day12 extends Day:

  type Input = String

  def parse(xs: List[String]): Input = xs.head

  def part1(input: Input) =
    val digits = """(-?\d+)""".r.unanchored
    digits.findAllIn(input).map(_.toLong).sum

  def part2(input: Input) =
    val json = ujson.read(input)

    def recurse(curr: ujson.Value): Long =
      curr match
        case ujson.Num(value) => value.toLong
        case Arr(value)       => value.iterator.map(recurse).sum
        case Obj(value) =>
          val hasRed = value.values
            .exists:
              case ujson.Str("red") => true
              case _                => false
          if hasRed then 0
          else value.values.map(recurse).sum
        case _ => 0

    recurse(json)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
