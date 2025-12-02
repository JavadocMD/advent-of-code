package aoc2024

import scala.annotation.tailrec
import aoc.Day
import aoc2024.Util._

object Day21 extends Day:

  type Input = List[String]

  def parse(xs: List[String]): Input = xs

  val numPad = Map(
    '0' -> Vector2(1, 3),
    'A' -> Vector2(2, 3),
    '1' -> Vector2(0, 2),
    '2' -> Vector2(1, 2),
    '3' -> Vector2(2, 2),
    '4' -> Vector2(0, 1),
    '5' -> Vector2(1, 1),
    '6' -> Vector2(2, 1),
    '7' -> Vector2(0, 0),
    '8' -> Vector2(1, 0),
    '9' -> Vector2(2, 0),
  )

  val dirPad = Map(
    '^' -> Vector2(1, 0),
    'A' -> Vector2(2, 0),
    '<' -> Vector2(0, 1),
    'v' -> Vector2(1, 1),
    '>' -> Vector2(2, 1),
  )

  def numButtons(from: Vector2, to: Vector2): List[List[Char]] =
    // Generate the options for moving `from` -> `to` on the num pad, avoiding the blank space
    // (Seems safe to assume that when both horizontal and vertical movement are required,
    // that we would want to do all of one and then all the other, "up-up-left-left" rather than "up-left-up-left".)
    val delta   = to - from
    lazy val dh = List.fill(delta.x.abs)(if delta.x < 0 then '<' else '>')
    lazy val dv = List.fill(delta.y.abs)(if delta.y < 0 then '^' else 'v')
    (from, to, delta) match
      // avoid corner: move up before left
      case (Vector2(_, 3), Vector2(0, _), _) => List(dv ::: dh ::: 'A' :: Nil)
      // avoid corner: move right before down
      case (Vector2(0, _), Vector2(_, 3), _) => List(dh ::: dv ::: 'A' :: Nil)
      case (_, _, Vector2(0, _))             => List(dv ::: 'A' :: Nil)
      case (_, _, Vector2(_, 0))             => List(dh ::: 'A' :: Nil)
      // try both horizontal- and vertical-first
      case _ => List(dh ::: dv ::: 'A' :: Nil, dv ::: dh ::: 'A' :: Nil)

  def dirButtons(from: Vector2, to: Vector2): List[List[Char]] =
    // Generate the options for moving `from` -> `to` on the dir pad, avoiding the blank space
    val delta   = to - from
    lazy val dh = List.fill(delta.x.abs)(if delta.x < 0 then '<' else '>')
    lazy val dv = List.fill(delta.y.abs)(if delta.y < 0 then '^' else 'v')
    (from, to, delta) match
      // avoid corner: move down before left
      case (Vector2(_, 0), Vector2(0, _), _) => List(dv ::: dh ::: 'A' :: Nil)
      // avoid corner: move right before up
      case (Vector2(0, _), Vector2(_, 0), _) => List(dh ::: dv ::: 'A' :: Nil)
      case (_, _, Vector2(0, _))             => List(dv ::: 'A' :: Nil)
      case (_, _, Vector2(_, 0))             => List(dh ::: 'A' :: Nil)
      // try both horizontal- and vertical-first
      case _ => List(dh ::: dv ::: 'A' :: Nil, dv ::: dh ::: 'A' :: Nil)

  def complexity(code: String, robots: Int): Long =
    val memoB = scala.collection.mutable.Map.empty[(Vector2, Vector2, Int), Long]

    def pairSolution(src: Vector2, dst: Vector2, stage: Int): Long =
      // Discovers the best solution for a pair of buttons at a particular stage;
      // while there are more stages "above" this one, recursively expand
      // to count the buttons that it would take to press these buttons.
      // We need to memoize the pair solution because the sequences will
      // get *very* long in part 2 but there's not that many unique sequences
      // to worry about at each stage.
      val pad     = if stage == 0 then numPad else dirPad
      val buttons = if stage == 0 then numButtons else dirButtons
      buttons(src, dst)
        .map: keys =>
          if stage < robots then sequenceSolution(keys, stage + 1)
          else keys.length.toLong
        .min

    def sequenceSolution(buttons: List[Char], stage: Int): Long =
      // Generates the best solution for a sequence of buttons
      // by examining each pair
      val pad = if stage == 0 then numPad else dirPad
      ('A' :: buttons)
        .map(pad)
        .sliding(2)
        .collect:
          case a :: b :: Nil => memoB.getOrElseUpdate((a, b, stage), pairSolution(a, b, stage))
        .sum

    sequenceSolution(code.toList, 0) * code.init.toLong

  def part1(input: Input) = input.map(complexity(_, 2)).sum

  def part2(input: Input) = input.map(complexity(_, 25)).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
