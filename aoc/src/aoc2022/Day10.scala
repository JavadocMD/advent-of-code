package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day10 extends Day:
  type Input = List[String]
  def parse(xs: Iterator[String]): Input =
    // addx takes two cycles, which we can more easily simulate by inserting a noop before each addx
    var acc = List.empty[String]
    while xs.hasNext do
      acc = xs.next match
        case "noop" => "noop" :: acc
        case add    => add :: "noop" :: acc
    acc.reverse

  case class State(cycle: Int, registerX: Int):
    def compute(instruction: String): State = instruction match
      case "noop"     => State(cycle + 1, registerX)
      case s"addx $n" => State(cycle + 1, registerX + n.toInt)

  object State:
    val initial = State(1, 1)

  def part1(input: Input) =
    val sampleCycles = Set(20, 60, 100, 140, 180, 220)
    val z            = (State.initial, 0)
    val (_, signal) = input.foldLeft(z) { case ((state, signal), instruction) =>
      val sample =
        if sampleCycles.contains(state.cycle)
        then state.cycle * state.registerX
        else 0
      (state.compute(instruction), signal + sample)
    }
    signal
  end part1

  object Screen:
    val width  = 40
    val height = 6
    val blank  = Vector.fill(width * height)(' ')
    def stringify(xs: Vector[Char]): String =
      xs.grouped(width).map(_.mkString).mkString("\n")

  def part2(input: Input) =
    val z = (State.initial, Screen.blank)
    val (_, screen) = input.foldLeft(z) { case ((state, screen), instruction) =>
      val cursor = state.cycle - 1
      val sprite = (state.registerX - cursor % Screen.width).abs <= 1
      (state.compute(instruction), if sprite then screen.updated(cursor, '#') else screen)
    }
    Screen.stringify(screen)
  end part2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
