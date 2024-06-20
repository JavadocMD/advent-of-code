package aoc2020

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.{mutable => m}

import aoc.Day

object Day15 extends Day:
  type Input = Array[Int]
  def parse(xs: Array[String]): Input = xs.head.split(",").map(_.toInt)

  def play(input: Array[Int], totalTurns: Int): Int =
    // Doing this with HashMap is about 10x slower,
    // but obviously requires fiddling to allocate a large-enough array.
    // val mem = new m.HashMap[Int, Int](10_000_000, 0.75d)
    val mem  = Array.fill[Int](30000000)(-1)
    var turn = 0

    input
      .dropRight(1)
      .foreach(n => {
        mem(n) = turn
        turn += 1
      })

    var next = input.last

    while (turn < totalTurns - 1) {
      val lastSpoken = {
        val tmp = mem(next)
        if (tmp < 0) turn else tmp
      }
      mem(next) = turn
      next = turn - lastSpoken
      turn += 1
    }

    next
  end play

  def part1(input: Input): Int = play(input, 2020)

  def part2(input: Input): Int = play(input, 30000000)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
