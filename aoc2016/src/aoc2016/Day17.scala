package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Util._
import java.security.MessageDigest

object Day17 extends Day:

  lazy val input = loadInput().head

  extension (s: Dir4) //
    def toLetter = s match
      case N => 'U'
      case S => 'D'
      case W => 'L'
      case E => 'R'

  val md5 = MessageDigest.getInstance("MD5")

  case class State(position: Vector2, steps: String):
    def hash =
      md5.update(input.getBytes)
      md5.update(steps.getBytes)
      val b = md5.digest()
      List(
        (b(0) >> 4) & 0x0f,
        (b(0) >> 0) & 0x0f,
        (b(1) >> 4) & 0x0f,
        (b(1) >> 0) & 0x0f,
      )

  val bounds = Rectangle(Vector2.zero, Vector2(3, 3))
  val all4   = List[Dir4](N, S, W, E) // order of directions matters!

  def neighbors(state: State): List[State] =
    for
      (hashValue, dir) <- state.hash.zip(all4)
      if hashValue > 0xa
      nextPos = state.position + dir.vector
      if bounds.contains(nextPos)
    yield
      val nextSteps = state.steps + dir.toLetter
      State(nextPos, nextSteps)

  def findAllPaths(start: Vector2, goal: Vector2): List[State] =
    @tailrec
    def recurse(frontier: List[State], acc: List[State] = Nil): List[State] =
      frontier match
        case Nil => acc
        case head :: tail if head.position == goal =>
          recurse(tail, head :: acc)
        case head :: tail =>
          recurse(neighbors(head) ::: tail, acc)
    recurse(State(start, "") :: Nil)

  lazy val allPaths = findAllPaths(Vector2.zero, Vector2(3, 3))

  lazy val part1 = allPaths.minBy(_.steps.size).steps

  lazy val part2 = allPaths.maxBy(_.steps.size).steps.size

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
