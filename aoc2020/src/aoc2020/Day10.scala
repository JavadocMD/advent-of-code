package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day10 extends Day:
  type Input = List[Int]
  def parse(xs: Array[String]): Input = xs.view.map(_.toInt).toList

  def difference(x: (Int, Int)): Int = x._2 - x._1

  def deltas(input: List[Int]): List[Int] = {
    val ratings = {
      val xs = input.sorted
      0 +: xs :+ (xs.last + 3) // prepend/append our endpoints
    }
    pairsIterator(ratings).map(difference).toList
  }

  def part1(input: List[Int]): Int = {
    val ds = deltas(input)
    ds.count(_ == 1) * ds.count(_ == 3)
  }

  def longestSequence1(xs: List[Int], curr: Int = 0, acc: Int = 0): Int = {
    xs match {
      case Nil       => acc
      case 1 :: tail => longestSequence1(tail, curr + 1, math.max(curr + 1, acc))
      case _ :: tail => longestSequence1(tail, 0, acc)
    }
  }

  def combos(size: Int): Int = {
    if (size <= 1) 1
    else {
      val (bottom, mid, top) = (1, List.range(2, size), size)
      val xs = for {
        n <- 0 to (size - 2)
        c <- mid.combinations(n)
        nums  = bottom +: c :+ top
        diffs = pairsIterator(nums).map(difference)
        if diffs.max <= 3
      } yield 1
      xs.sum
    }
  }

  // Every time there is a 3 in the diff list, this represents a
  // forced choice of those two numbers. The "choice tree" funnels
  // down to 1 possibility at these points. Hence we can imagine dividing
  // the larger list at these points into many smaller lists to
  // divide and conquer. If we denote the number of choices for list [A] as C([A]):
  //   C([A] ++ [B]) = C([A]) * C([B])
  // assuming [A] and [B] are separated by a delta of 3.
  def part2(input: List[Int]): Long = {
    val ds = deltas(input)
    // We can also exploiting some observations on the input set:
    // - all deltas are either 1 or 3
    // println(ds.distinct.mkString(","))
    // - the longest run of 1's is 4, corresponding to a sublist of 5 numbers
    // println(longestSequence1(ds))

    // Rather than figure out a mathematical relationship here, we can hard code
    // the number of choices for sublists of any given length.
    // n should be `(# of consecutive delta 1s) + 1`
    def choices(n: Int): Int = {
      n match {
        case 1 => 1 // combos(1)
        case 2 => 1 // combos(2)
        case 3 => 2 // combos(3)
        case 4 => 4 // combos(4)
        case 5 => 7 // combos(5)
        case _ => throw new Exception(s"Unhandled sublist size ($n).")
      }
    }

    // Finally, scan the list for sequences and multiply 'em up.
    def calcChoices(xs: List[Int], n: Int = 0, acc: Long = 1): Long = {
      xs match {
        case Nil       => acc * choices(n + 1)
        case 1 :: tail => calcChoices(tail, n + 1, acc)
        case 3 :: tail => calcChoices(tail, 0, acc * choices(n + 1))
        case n :: tail => throw new Exception(s"Unexpected delta ($n).")
      }
    }

    calcChoices(ds)
  }

  // There's also this solution: the number of choices to get to each adapter
  // equals the number of choices to get to all of the previous adapters whose
  // value is within 3. So you can simply keep a running sum.
  // https://todd.ginsberg.com/post/advent-of-code/2020/day10/
  def part2b(input: List[Int]): Long = {
    val z = scala.collection.immutable.ListMap(0 -> 1L).withDefault(_ => 0L)
    (input.sorted :+ input.max + 3)
      .foldLeft(z)({ case (c, i) =>
        val x = c(i - 1) + c(i - 2) + c(i - 3)
        c.updated(i, x)
      })
      .values
      .last
  }

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
