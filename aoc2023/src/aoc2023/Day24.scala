package aoc2023

import scala.annotation.tailrec
import org.apache.commons.math3.linear.Array2DRowRealMatrix
import org.apache.commons.math3.linear.MatrixUtils
import aoc.Day

object Day24 extends Day:

  case class Vector3(x: Long, y: Long, z: Long)

  case class Stone(position: Vector3, velocity: Vector3):
    // These properties and methods are for the 2D assumption in part 1.
    val slope = velocity.y.toDouble / velocity.x
    val y0    = position.y - slope * position.x

    def isInFuture(x: Double, y: Double): Boolean =
      math.signum(x - position.x) == math.signum(velocity.x) &&
        math.signum(y - position.y) == math.signum(velocity.y)

  type Input = List[Stone]

  def parse(xs: List[String]): Input =
    xs.map:
      case s"$x, $y, $z @ $vx, $vy, $vz" =>
        Stone(
          Vector3(x.toLong, y.toLong, z.toLong),
          Vector3(vx.toLong, vy.toLong, vz.toLong),
        )

  def intercept2D(a: Stone, b: Stone): (Double, Double) =
    val x = (b.y0 - a.y0) / (a.slope - b.slope) // no div by zero errors; I guess none of these are parallel!
    val y = a.slope * x + a.y0
    (x, y)

  def interceptsWithin(stones: List[Stone], min: Double, max: Double): Iterator[(Double, Double)] =
    for
      sublist <- stones.tails.takeWhile(_.nonEmpty)
      a = sublist.head
      b <- sublist.tail
      (x, y) = intercept2D(a, b)
      if x >= min && x <= max
      if y >= min && y <= max
      if a.isInFuture(x, y) && b.isInFuture(x, y)
    yield (x, y)

  def part1(input: Input) =
    val min = 200000000000000d
    val max = 400000000000000d
    interceptsWithin(input, min, max).size

  def part2(input: Input) =
    // Linear algebra time!
    // Just looking at a single stone and combining the x and y position
    // equations, we get a non-linear system of equations.
    // However by combining stones we can equate the non-linear elements
    // and produce a linear system of equations.

    // If our starting position is (xT,yT,zT) and throwing velocity is (vxT,vyT,vzT),
    // after doing a bunch of algebra for the below quantities, we get a system:
    // A * X = B

    // [ a1 b1 c1 d1 ] [  xT ] = [ s1 ]
    // [ a2 b2 c2 d2 ] [  yT ] = [ s2 ]
    // [ a3 b3 c3 d3 ] [ vxT ] = [ s3 ]
    // [ a4 b4 c4 d4 ] [ vyT ] = [ s4 ]

    // Combine pairs of stones and calculate matrix rows; we need four such pairs.
    // I don't think it matters which we pick, so I'll do a sliding window of length 2
    // over the first five stones.
    val rows = input
      .sliding(2)
      .map: pair =>
        val Stone(Vector3(x1, y1, _), Vector3(vx1, vy1, _)) = pair(0)
        val Stone(Vector3(x2, y2, _), Vector3(vx2, vy2, _)) = pair(1)

        val coeffs = Array(vy1 - vy2, vx2 - vx1, y2 - y1, x1 - x2)
        val consts = Array(vy1 * x1 - vx1 * y1 - vy2 * x2 + vx2 * y2)
        (coeffs.map(_.toDouble), consts.map(_.toDouble))
      .take(4)
      .toArray

    // Solving for X:
    // X = A_inverse * B
    val coeffs = new Array2DRowRealMatrix(rows.map(_._1))
    val consts = new Array2DRowRealMatrix(rows.map(_._2))
    val invers = MatrixUtils.inverse(coeffs)
    val solutn = invers.multiply(consts).getData()

    val Array(xT, yT, vxT, vyT) = solutn.flatten.map(_.round)

    val Stone(Vector3(x1, y1, z1), Vector3(vx1, vy1, vz1)) = input(0)
    val Stone(Vector3(x2, y2, z2), Vector3(vx2, vy2, vz2)) = input(1)

    // We can solve the x-equations for two stones to get t1 and t2.
    val t1 = (xT - x1) / (vx1 - vxT)
    val t2 = (xT - x2) / (vx2 - vxT)

    // And finally solve the z-equations for two stones to get zT and vzT.
    val vzT = (vz2 * t2 - vz1 * t1 + z2 - z1) / (t2 - t1)
    val zT  = t1 * (vz1 - vzT) + z1

    // Whew!
    println(s"($xT, $yT, $zT)")
    println(s"($vxT, $vyT, $vzT)")
    println(s"t1 = $t1; t2 = $t2")
    xT + yT + zT

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
