package aoc2019

import scala.io.Source
import scala.annotation.tailrec
import fastparse._
import NoWhitespace._

import aoc.Day

object Day06 extends Day:
  type Input = List[Orbit]

  def parse(xs: Array[String]): Input =
    @tailrec
    def fromLines(lines: List[String], acc: List[Orbit] = List()): List[Orbit] =
      lines match {
        case Nil => acc
        case head :: tail =>
          val curr = fastparse.parse(head, parseOrbit(_))
          if (!curr.isSuccess) throw new Exception("Could not parse input.")
          fromLines(tail, curr.get.value :: acc)
      }
    fromLines(xs.toList)

  def part1(input: Input): Int =
    val bodies = assembleBodies(input)
    countOrbits(bodies)

  def part2(input: Input): Int =
    val bodies = assembleBodies(input)
    countTransfers(bodies).get

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))

  case class Orbit(primary: String, satellite: String)

  def parseBody[$: P]  = P((!")" ~ AnyChar).rep.!)
  def parseOrbit[$: P] = P(parseBody ~ ")" ~ parseBody).map({ case (p, s) => Orbit(p, s) })

  // Heavenly bodies and their orbits can be represented as a tree.
  // Trees are hard. Instead, I store each body with its parent bodies in the tree.
  // From the example, H is: `Body(H, List(G, B, COM))`
  // The count of a body's direct and indirect orbits is the size of its parents list (depth).

  case class Body(name: String, parents: List[String]) {
    val depth            = parents.size
    def ::(name: String) = Body(name, this.name :: parents)
  }
  object Body {
    val Common = Body("COM", List())
  }

  // Starting with the set of all orbits and a working set containing the "COM" body,
  // recursively assemble the list of all bodies.
  // Each step:
  // - take the first body off the working set (curr),
  // - add curr to our result,
  // - find the bodies which orbit curr and add them to the working set,
  // - remove orbits that we've "used" in this way.
  // Finally: we should have exhausted both the working set and the orbits set.

  @tailrec
  final def assembleBodies(
      orbits: List[Orbit],
      work: List[Body] = List(Body.Common),
      acc: List[Body] = List()
  ): List[Body] = {
    (orbits, work) match {
      case (Nil, Nil) => acc.reverse
      case (_, Nil)   => throw new Exception("Did not exhaust orbits.")
      case (_, curr :: nextWork) =>
        val (orbitingCurr, nextOrbits) = orbits.partition(x => x.primary == curr.name)
        val newWork                    = orbitingCurr.map(x => x.satellite :: curr)
        // println(s"counting ${curr.name} (${curr.depth}): enqueue ${newWork.map(x => x.name)}")
        assembleBodies(nextOrbits, nextWork ::: newWork, curr :: acc)
    }
  }

  // Part One: the sum of orbits is just the sum of the depths of all bodies.

  def countOrbits(bodies: List[Body]): Int = bodies.foldRight(0)(_.depth + _)

  // Part Two: you and Santa have a common ancestor in the tree.
  // The total number of transfers is:
  // - the depth of your orbited body minus the depth of the common ancestor
  //   plus
  // - the depth of Santa's orbited body minus the depth of the common ancestor

  def commonSuffix[T](a: List[T], b: List[T]): List[T] = {
    a.reverse
      .zip(b.reverse)
      .takeWhile({ case (a, b) => a == b })
      .map({ case (a, b) => a })
      .reverse
  }

  def commonAncestor(a: Body, b: Body): Body = {
    if (a.name == b.name) a
    else
      commonSuffix(a.parents, b.parents) match {
        case head :: tail => Body(head, tail)
        case Nil          => throw new Exception("No common ancestor found.")
      }
  }

  def countTransfers(bodies: List[Body]): Option[Int] = {
    for {
      you <- bodies.find(x => x.name == "YOU")
      san <- bodies.find(x => x.name == "SAN")
      com = commonAncestor(you, san)
    } yield (you.depth - 1 - com.depth) + (san.depth - 1 - com.depth)
  }
