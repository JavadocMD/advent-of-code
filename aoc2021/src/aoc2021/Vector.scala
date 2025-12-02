package aoc2021

case class Vector(x: Int, y: Int):
  def +(that: Vector) = Vector(x + that.x, y + that.y)

object Vector:
  val zero = Vector(0, 0)

case class VectorBounds(minX: Int, maxX: Int, minY: Int, maxY: Int):
  assert(minX <= maxX && minY <= maxY, "VectorBounds construction must respect min and max constraint.")
  def contains(v: Vector): Boolean =
    v.x >= minX && v.x <= maxX && v.y >= minY && v.y <= maxY

object VectorBounds:
  def from(x0: Int, x1: Int, y0: Int, y1: Int): VectorBounds =
    VectorBounds(Math.min(x0, x1), Math.max(x0, x1), Math.min(y0, y1), Math.max(y0, y1))

  object Ops:
    extension (v: Vector)
      def isWithin(b: VectorBounds): Boolean  = b.contains(v)
      def isLeftOf(b: VectorBounds): Boolean  = v.x < b.minX
      def isRightOf(b: VectorBounds): Boolean = v.x > b.maxX
      def isAbove(b: VectorBounds): Boolean   = v.y > b.maxY
      def isBelow(b: VectorBounds): Boolean   = v.y < b.minY
