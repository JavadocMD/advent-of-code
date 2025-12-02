package aoc2019

import aoc._
import scala.io.Source
import scala.annotation.tailrec
import scala.math.{abs, atan2}

import aoc.Day

object Day10 extends Day:
  type Input = SpaceMap

  def parse(xs: Array[String]): Input =
    if (xs.size == 0) throw new Exception("Error parsing map: there are no rows.")

    val size = Vector(xs(0).size, xs.size)
    val data = for {
      (r, y) <- xs.zipWithIndex
      (c, x) <- r.toCharArray.zipWithIndex
      v      <- if (c == '#') Some(Vector(x, y)) else None
    } yield v

    new SpaceMap(size, data.toSet)

  implicit class SpaceMapHelper(private val sc: StringContext) extends AnyVal:
    def m(args: Any*): Array[String] = sc.raw(args: _*).stripMargin.trim.linesIterator.toArray

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  val π2    = 2d * Math.PI
  val π3of2 = 3d * Math.PI / 2d

  case class Vector(x: Int, y: Int) {
    def +(that: Vector) = Vector(this.x + that.x, this.y + that.y)
    def -(that: Vector) = Vector(this.x - that.x, this.y - that.y)
    def *(f: Int)       = Vector(this.x * f, this.y * f)
    def /(f: Int)       = Vector(this.x / f, this.y / f)

    /** * Angle in radians between 0 and 2π, with 0 along the +y-axis.
      */
    def angle: Double = ((5d * Math.PI / 2d) - atan2(-this.y, this.x)) % (Math.PI * 2d)

    /** * If this vector were expressed as the fraction `x / y`, reduce it to its lowest terms.
      */
    def reduce: Vector = if (this == Vector.zero) this else this / gcd(abs(this.x), abs(this.y))
  }

  object Vector:
    val zero = Vector(0, 0)

    def deltaBetween(a: Vector, b: Vector): Vector = (b - a).reduce

    /** * A stream of all of the points that lie exactly on the path between `from` and `to`, excluding the endpoints.
      */
    def pointsBetween(a: Vector, b: Vector): Stream[Vector] = {
      val Δ                           = deltaBetween(a, b)
      lazy val points: Stream[Vector] = (b - Δ) #:: points.map(_ - Δ)
      points.takeWhile(_ != a)
    }

    def angleBetween(a: Vector, b: Vector): Double = deltaBetween(a, b).angle
  end Vector

  class SpaceMap(val size: Vector, val asteroids: Set[Vector]):

    /** * Is there an asteroid at `v`?
      */
    def isAsteroid(v: Vector) = asteroids.contains(v)

    /** * Is there an uninterrupted line of sight between `from` and `to`?
      */
    def canSee(from: Vector, to: Vector): Boolean = Vector.pointsBetween(from, to).find(isAsteroid _).isEmpty

    /** * How many asteroids can be seen from `from`?
      */
    def countVisible(from: Vector): Int = (asteroids - from).count(canSee(from, _))

    /** * How many asteroids are between `from` and `to`?
      */
    def intervening(from: Vector, to: Vector): Int = Vector.pointsBetween(from, to).count(isAsteroid _)

  end SpaceMap

  def bestMonitor(m: SpaceMap): (Vector, Int) = {
    m.asteroids.map(v => (v, m.countVisible(v))).maxBy(_._2)
  }

  case class Target(asteroid: Vector, intervening: Int, angle: Double) extends Ordered[Target] {
    import scala.math.Ordered.orderingToOrdered
    def compare(that: Target) = (this.intervening, this.angle) compare (that.intervening, that.angle)
  }

  def targetOrder(m: SpaceMap, from: Vector) = {
    def toTarget(v: Vector) = Target(v, m.intervening(from, v), Vector.angleBetween(from, v))
    (m.asteroids - from).toSeq.map(toTarget _).sorted
  }

  def part1(input: Input): Long =
    val (pos, n) = bestMonitor(input)
    n

  def part2(input: Input): Long =
    val (pos, n) = bestMonitor(input)
    val winner   = targetOrder(input, pos)(199).asteroid
    winner.x * 100 + winner.y

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
