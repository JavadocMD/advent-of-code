package aoc2021

import scala.annotation.tailrec

import aoc.Day

object Day02 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  def part1(input: Array[String]) =
    var position = 0
    var depth    = 0
    for s <- input do
      s.split(" ") match
        case Array("up", num)      => depth -= num.toInt
        case Array("down", num)    => depth += num.toInt
        case Array("forward", num) => position += num.toInt
        case _                     =>
    position * depth

  def part2(input: Array[String]) =
    var position = 0
    var depth    = 0
    var aim      = 0
    for s <- input do
      s.split(" ") match
        case Array("up", num)   => aim -= num.toInt
        case Array("down", num) => aim += num.toInt
        case Array("forward", num) =>
          position += num.toInt
          depth += num.toInt * aim
        case _ =>
    position * depth

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
