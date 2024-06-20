package aoc2020

import scala.io.Source
import scala.annotation.tailrec
import math.abs

import aoc.Day

object Day12 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  def direction(degrees: Int): Char = degrees match
    case 270 => 'N'
    case 0   => 'E'
    case 90  => 'S'
    case 180 => 'W'

  def part1(input: Input): Int =
    var x, y, r = 0
    input.foreach { s =>
      val n = s.tail.toInt
      val c = s.head match {
        case 'F' => direction(r)
        case x   => x
      }
      c match {
        case 'N' => y -= n
        case 'E' => x += n
        case 'S' => y += n
        case 'W' => x -= n
        case 'L' => r = (r + 360 - n) % 360
        case 'R' => r = (r + n) % 360
      }
    }
    abs(x) + abs(y)

  def part2(input: Input): Int =
    var x, y   = 0       // ship
    var (i, j) = (10, 1) // waypoint

    def rotate90(): Unit =
      val (i1, j1) = (j, -i)
      i = i1; j = j1

    input.foreach { s =>
      var n = s.tail.toInt
      s.head match {
        case 'N' => j += n
        case 'E' => i += n
        case 'S' => j -= n
        case 'W' => i -= n
        case 'L' =>
          n = 360 - n
          while (n > 0) { rotate90(); n -= 90 }
        case 'R' =>
          while (n > 0) { rotate90(); n -= 90 }
        case 'F' =>
          x += i * n
          y += j * n
      }
    }
    abs(x) + abs(y)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
