package aoc2019

import scala.annotation.tailrec

object ThreeDee:
  import scala.math.abs
  import TwoDee.{Vector => Vector2}

  case class Vector(x: Int, y: Int, z: Int):
    def +(that: Vector) = Vector(this.x + that.x, this.y + that.y, this.z + that.z)

    def -(that: Vector) = Vector(this.x - that.x, this.y - that.y, this.z - that.z)

    def scale(f: Int) = Vector(this.x * f, this.y * f, this.z * f)

    def distance(that: Vector): Int = abs(this.x - that.x) + abs(this.y - that.y) + abs(this.z - that.z)

    def neighbors: Seq[Vector] = Seq(
      Vector(x, y + 1, z),
      Vector(x, y - 1, z),
      Vector(x + 1, y, z),
      Vector(x - 1, y, z),
      Vector(x, y, z + 1),
      Vector(x, y, z - 1),
    )

    /** Neighbors in the XY-plane only. */
    def neighborsXY: Seq[Vector] = Seq(
      Vector(x, y + 1, z),
      Vector(x, y - 1, z),
      Vector(x + 1, y, z),
      Vector(x - 1, y, z),
    )

  object Vector:
    val zero = Vector(0, 0, 0)

    def withZ(v2: Vector2, z: Int): Vector = Vector(v2.x, v2.y, z)
    def to2D(v3: Vector): Vector2          = Vector2(v3.x, v3.y)
