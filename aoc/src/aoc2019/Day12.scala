package aoc2019

import aoc._
import scala.annotation.tailrec
import scala.math.abs

import aoc.Day

object Day12 extends Day:
  type Input = Seq[Moon]

  def parseInput(xs: Seq[String]): Seq[Moon] =
    xs.map({ case s"<x=$x, y=$y, z=$z>" =>
      Moon(
        position = Vector(x.trim.toInt, y.trim.toInt, z.trim.toInt),
        velocity = Vector.zero
      )
    })

  def parseMoons(xs: Seq[String]): Seq[Moon] =
    xs.map({ case s"pos=<x=$x, y=$y, z=$z>, vel=<x=$vx, y=$vy, z=$vz>" =>
      Moon(
        position = Vector(x.trim.toInt, y.trim.toInt, z.trim.toInt),
        velocity = Vector(vx.trim.toInt, vy.trim.toInt, vz.trim.toInt)
      )
    })

  def parse(xs: Array[String]): Input = parseInput(xs.toSeq)

  @tailrec
  final def gcd(a: Long, b: Long): Long = if (b == 0) abs(a) else gcd(b, a % b)
  final def lcm(a: Long, b: Long)       = abs(a * b) / gcd(a, b)

  case class Vector(x: Int, y: Int, z: Int):
    def +(that: Vector) = Vector(this.x + that.x, this.y + that.y, this.z + that.z)
    def -(that: Vector) = Vector(this.x - that.x, this.y - that.y, this.z - that.z)
    def absSum          = abs(this.x) + abs(this.y) + abs(this.z)

  object Vector:
    val zero = Vector(0, 0, 0)

  case class Moon(position: Vector, velocity: Vector):

    /** Calculates the effect of gravity on this moon from another moon.
      */
    def gravity(that: Moon): Vector = Vector(
      that.position.x compare this.position.x,
      that.position.y compare this.position.y,
      that.position.z compare this.position.z
    )

    /** * Move under the given force of gravity (and previous velocity).
      */
    def move(gravity: Vector): Moon =
      val v = velocity + gravity
      this.copy(velocity = v, position = position + v)

    def energy = position.absSum * velocity.absSum

  def moonsWithGravity(moons: Seq[Moon]): Seq[(Moon, Vector)] = for {
    m <- moons
    g = moons.map(m gravity _).reduce(_ + _)
  } yield (m, g)

  def step(moons: Seq[Moon]): Seq[Moon] =
    moonsWithGravity(moons).map({ case (m, g) => m.move(g) })

  @tailrec
  final def stepN(moons: Seq[Moon], n: Long): Seq[Moon] =
    if (n == 0) moons
    else stepN(step(moons), n - 1)

  /** For part two, we need a much more efficient solution. First, realize that each axis is independent. Further, each
    * step is deterministic given the step preceding it -- thus if any step reoccurs exactly, it will indicate a
    * repeating cycle. The period of the whole function -- which is the sum of independent periodic functions -- is the
    * least common multiple of the periods of each independent function. Thus, we can compute the periods of each axis
    * separately, and the resulting LCM is our answer. Also we can assume we are only dealing with 4 moon systems.
    */
  object SingleAxis:
    case class State(m0: Int, m1: Int, m2: Int, m3: Int, v0: Int, v1: Int, v2: Int, v3: Int)

    /** Returns how many steps it takes to repeat. */
    def findCycle(moons: Seq[Int]): Long = {
      // To make this as fast as possible we use mutable ints and unrolled loops.
      assert(moons.size == 4)
      var (m0, m1, m2, m3) = (moons(0), moons(1), moons(2), moons(3))
      var v0, v1, v2, v3   = 0

      // This is basically a slow, but perfect, hashing method.
      def toState() = State(m0, m1, m2, m3, v0, v1, v2, v3)

      val hashes = new scala.collection.mutable.HashMap[State, Long](524288, 0.75d)
      hashes += toState() -> 0L

      var i    = 0L
      var j    = 0L
      var done = false
      while (!done) {
        i += 1

        // Update velocity due to gravity.

        if (m0 < m1) {
          v0 += 1
          v1 -= 1
        } else if (m0 > m1) {
          v0 -= 1
          v1 += 1
        }

        if (m0 < m2) {
          v0 += 1
          v2 -= 1
        } else if (m0 > m2) {
          v0 -= 1
          v2 += 1
        }

        if (m0 < m3) {
          v0 += 1
          v3 -= 1
        } else if (m0 > m3) {
          v0 -= 1
          v3 += 1
        }

        if (m1 < m2) {
          v1 += 1
          v2 -= 1
        } else if (m1 > m2) {
          v1 -= 1
          v2 += 1
        }

        if (m1 < m3) {
          v1 += 1
          v3 -= 1
        } else if (m1 > m3) {
          v1 -= 1
          v3 += 1
        }

        if (m2 < m3) {
          v2 += 1
          v3 -= 1
        } else if (m2 > m3) {
          v2 -= 1
          v3 += 1
        }

        // Apply velocity.

        m0 += v0
        m1 += v1
        m2 += v2
        m3 += v3

        // Check for hash collision.
        // If our hash is perfect, we can stop at the first collision.
        val s = toState()
        if (hashes.contains(s)) {
          done = true
          j = hashes.getOrElse(s, 0L)
        }
        hashes += s -> i
      }

      i - j
    }
  end SingleAxis

  def findCycle(moons: Seq[Moon]): Long =
    val x = SingleAxis.findCycle(moons.map(_.position.x))
    val y = SingleAxis.findCycle(moons.map(_.position.y))
    val z = SingleAxis.findCycle(moons.map(_.position.z))
    lcm(x, lcm(y, z))

  def part1(input: Input): Long = stepN(input, 1000).map(_.energy).sum

  def part2(input: Input): Long = findCycle(input)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
