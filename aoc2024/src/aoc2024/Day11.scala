package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day11 extends Day:

  type Input = List[Long]

  def parse(xs: List[String]): Input = xs.head.split(" ").map(_.toLong).toList

  object digitRule:
    def unapply(stone: Long): Option[List[Long]] =
      val digits = if stone == 0 then 1 else math.log10(math.abs(stone)).toInt + 1
      if digits % 2 != 0 then None
      else
        val divisor = math.pow(10, digits / 2).toInt
        Some(List(stone / divisor, stone % divisor))

  def blink(stone: Long): List[Long] =
    stone match
      case 0L                   => List(1L)
      case digitRule(newStones) => newStones
      case _                    => List(stone * 2024L)

  def part1(input: Input) =
    // Iterator
    //   .range(0, 25)
    //   .foldLeft(input): (stones, _) =>
    //     stones.flatMap(blink)
    //   .size
    input.map(blinkN(_, 25)).sum

  val memo = scala.collection.mutable.Map.empty[(Long, Int), Long]

  def blinkN(stone: Long, times: Int): Long =
    memo.getOrElseUpdate(
      key = (stone, times),
      defaultValue = if times == 0 then 1 else blink(stone).map(blinkN(_, times - 1)).sum
    )

  def part2(input: Input) = input.map(blinkN(_, 75)).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
