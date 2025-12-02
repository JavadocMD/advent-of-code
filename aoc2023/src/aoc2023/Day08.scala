package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day08 extends Day:

  type Rooms = Map[String, (String, String)]

  case class Input(instructions: List[Char], rooms: Rooms):
    lazy val instructionCycle: LazyList[Char] =
      LazyList.from(instructions) #::: instructionCycle

  def parse(xs: List[String]): Input =
    val instructions = xs.head.toList
    val rooms = xs.tail.tail.map { case s"$room = ($left, $right)" =>
      room -> (left, right)
    }
    Input(instructions, rooms.toMap)

  def steps(start: String, end: String, input: Input, instructions: LazyList[Char]): Option[(Long, LazyList[Char])] =
    val n = input.instructions.size
    @tailrec def recurse(
        room: String,
        ins: LazyList[Char],
        roomSteps: Set[(String, Long)],
        steps: Long,
    ): Option[(Long, LazyList[Char])] =
      val curr = (room, steps % n)
      if room == end && steps > 0 then Some((steps, ins))
      else if roomSteps.contains(curr) then None // loop detection
      else
        val head #:: tail = ins: @unchecked
        val nextRoom = head match
          case 'L' => input.rooms(room)._1
          case 'R' => input.rooms(room)._2
        recurse(nextRoom, tail, roomSteps + curr, steps + 1)

    recurse(start, instructions, Set.empty, 0)

  def part1(input: Input) =
    steps("AAA", "ZZZ", input, input.instructionCycle).get._1

  def part2(input: Input) =
    val starts = input.rooms.keys.filter(_.endsWith("A")).toList
    val ends   = input.rooms.keys.filter(_.endsWith("Z")).toList

    val xs = for
      s             <- starts
      e             <- ends
      (steps1, ins) <- steps(s, e, input, input.instructionCycle)
      (steps2, _)   <- steps(e, e, input, ins)
    yield (s, steps1, steps2)

    println(xs.mkString("\n"))
    // The results from the above step were inspected visually
    // from which I drew a few conclusions:
    // 1. each A only passes through one Z
    // 2. after reaching Z, there is a loop that leads back to Z
    // 3. the distance (steps) to go Z->Z is the same as for A->Z
    //    in every case; the input must have been carefully crafted
    //    to make this so
    // Therefore: we can simply do the LCM of the five A->Z distances.

    xs.map(_._2).reduce(lcm)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
