package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day12 extends Day:

  sealed trait Spring
  case object Working extends Spring
  case object Damaged extends Spring
  case object Unknown extends Spring

  case class Row(springs: List[Spring], springText: String, groups: List[Int]) // groups are damaged runs

  type Input = List[Row]

  def parseRow(s: String): Row = s match
    case s"$springs $groups" =>
      Row(
        springs.map {
          case '.' => Working
          case '#' => Damaged
          case '?' => Unknown
        }.toList,
        springs,
        groups.split(",").view.map(_.toInt).toList,
      )

  def parse(xs: List[String]): Input = xs.map(parseRow)

  sealed trait FitResult
  case object StopFit                          extends FitResult
  case object NoFit                            extends FitResult
  case class Possible(remaining: List[Spring]) extends FitResult
  case class Required(remaining: List[Spring]) extends FitResult

  def fit(springs: List[Spring], length: Int): FitResult =
    if springs.size < length then NoFit
    else {
      val (left, right) = springs.splitAt(length) // split in two at length

      // If left starts with Damaged then either we match here or fail the whole chain
      // Otherwise, `possible` can keep searching by shifting right.
      val badResult = if left.head == Damaged then StopFit else NoFit

      if left.exists(_ == Working) then badResult  // can't have any Working on the left
      else if right.isEmpty then Required(Nil)     // right can be empty
      else if right.head == Damaged then badResult // can't start with Damaged on the right
      else {
        if left.head == Damaged then Required(right.tail) else Possible(right.tail)
      }
    }

  def possible(row: Row): Long =
    val memo = scala.collection.mutable.Map.empty[(Int, Int), Long]

    def memoized(springs: List[Spring], groups: List[Int]): Long =
      memo.getOrElseUpdate(
        (springs.size, groups.size), //
        recurse(springs, groups),
      )

    def recurse(springs: List[Spring], groups: List[Int]): Long =
      val minLength = groups.sum + groups.size - 1

      if groups.isEmpty then {
        if springs.exists(_ == Damaged) then 0 else 1
        // } else if springs.isEmpty then {
      } else if springs.size < minLength then {
        0
      } else {
        fit(springs, groups.head) match
          case StopFit             => 0
          case NoFit               => memoized(springs.tail, groups)
          case Possible(remaining) => memoized(springs.tail, groups) + memoized(remaining, groups.tail)
          case Required(remaining) => memoized(remaining, groups.tail)
      }

    memoized(row.springs, row.groups)

  type Match = List[(Int, Int)]

  def matchString(r: Row, m: Match): String =
    val n = r.springs.size
    val ds = m.foldLeft(Set.empty[Int]):
      case (acc, (index, length)) => acc ++ (index until index + length)

    val chars = for
      i <- 0 until n
      char = if ds.contains(i) then '#' else '.'
    yield char
    chars.mkString

  def debugPossible(row: Row): List[Match] =
    val n = row.springs.size
    // matches are (index, group length)
    def recurse(springs: List[Spring], groups: List[Int], acc: Match): List[Match] =
      if groups.isEmpty then {
        if springs.exists(_ == Damaged) then Nil else List(acc)
      } else if springs.isEmpty then {
        Nil
      } else {
        val length = groups.head
        val index  = n - springs.size
        fit(springs, length) match
          case StopFit => Nil
          case NoFit   => recurse(springs.tail, groups, acc)
          case Possible(remaining) =>
            recurse(springs.tail, groups, acc) ::: recurse(remaining, groups.tail, (index, length) :: acc)
          case Required(remaining) => recurse(remaining, groups.tail, (index, length) :: acc)
      }
    recurse(row.springs, row.groups, Nil)

  def part1(input: Input) =
    input.map(possible).sum

  def unfoldRow(row: Row): Row = row match
    case Row(springs, springText, groups) =>
      val xs  = (0 until 5).toList.flatMap(_ => Unknown :: springs).tail
      val txt = (0 until 5).toList.map(_ => '?' + springText).mkString.tail
      val gs  = (0 until 5).toList.flatMap(_ => groups)
      Row(xs, txt, gs)

  def part2(input: Input) =
    input.map(unfoldRow).map(possible).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
