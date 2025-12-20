package aoc2015

import scala.annotation.tailrec
import aoc.Day
import aoc2015.Util._
import scala.math.max

object Day06 extends Day:

  sealed trait Action
  case object TurnOn  extends Action
  case object TurnOff extends Action
  case object Toggle  extends Action

  case class Rule(action: Action, from: Vector2, through: Vector2):
    val xs = this.from.x to this.through.x
    val ys = this.from.y to this.through.y

    def contains(point: Vector2): Boolean = this.xs.contains(point.x) && this.ys.contains(point.y)

  type Input = List[Rule]

  def parse(xs: List[String]): Input = xs.map:
    case s"turn on $x0,$y0 through $x1,$y1"  => Rule(TurnOn, Vector2(x0.toInt, y0.toInt), Vector2(x1.toInt, y1.toInt))
    case s"turn off $x0,$y0 through $x1,$y1" => Rule(TurnOff, Vector2(x0.toInt, y0.toInt), Vector2(x1.toInt, y1.toInt))
    case s"toggle $x0,$y0 through $x1,$y1"   => Rule(Toggle, Vector2(x0.toInt, y0.toInt), Vector2(x1.toInt, y1.toInt))

  def part1(input: Input) =
    // Observing that what happens before a "turn on" or "turn off" is irrelevant,
    // we can process the rules in reverse order, keeping track of the number of toggles.
    val rules = input.reverse

    def isOn(rules: List[Rule], point: Vector2, toggles: Int = 0): Boolean =
      rules match
        case Nil                                   => toggles % 2 == 1
        case head :: tail if !head.contains(point) => isOn(tail, point, toggles)
        case Rule(TurnOn, _, _) :: tail            => toggles % 2 == 0
        case Rule(TurnOff, _, _) :: tail           => toggles % 2 == 1
        case Rule(Toggle, _, _) :: tail            => isOn(tail, point, toggles + 1)

    val lights = for
      j <- (0 until 1000).iterator
      i <- 0 until 1000
    yield Vector2(i, j)

    lights.count(isOn(rules, _))

  def part2(input: Input) =
    // The trick from part 1 doesn't apply this time, so we'll use mutable state instead.
    // Which iteration order is faster?
    // - Check every light, iterating through rules: 1M lights * 300 rules = 300M
    // - Check every rule, iterating through affected lights: total "area" of all rules (with overlaps) is ~20M
    // So we'll do the latter.
    val lights = Array.fill(1000 * 1000)(0)
    for
      r <- input.iterator
      j <- r.ys
      i <- r.xs
      index = j * 1000 + i
    do
      r.action match
        case TurnOn  => lights(index) += 1
        case TurnOff => lights(index) = max(0, lights(index) - 1)
        case Toggle  => lights(index) += 2
    lights.foldLeft(0L)(_ + _)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
