package aoc2017

case class Vector2(x: Int, y: Int) extends Ordered[Vector2]:
  def +(that: Vector2): Vector2     = Vector2(this.x + that.x, this.y + that.y)
  def -(that: Vector2): Vector2     = Vector2(this.x - that.x, this.y - that.y)
  def *(that: Vector2): Vector2     = Vector2(this.x * that.y, this.y * that.y)
  def *(that: Int): Vector2         = Vector2(this.x * that, this.y * that)
  def /(that: Vector2): Vector2     = Vector2(this.x / that.y, this.y / that.y)
  def /(that: Int): Vector2         = Vector2(this.x / that, this.y / that)
  def neighbors4: List[Vector2]     = Direction.all4.map(this + _.vector)
  def neighbors8: List[Vector2]     = Direction.all8.map(this + _.vector)
  def manhattan(that: Vector2): Int = (that.x - this.x).abs + (that.y - this.y).abs
  override def toString(): String   = s"$x,$y"

  def compare(that: Vector2): Int =
    val dx = this.x.compareTo(that.x)
    if dx != 0 then dx else this.y.compareTo(that.y)

object Vector2:
  val zero = Vector2(0, 0)

  def s(x: String, y: String): Vector2 = Vector2(x.toInt, y.toInt)

sealed trait Direction:
  val vector: Vector2
sealed trait Dir4 extends Direction
sealed trait Dir8 extends Direction
case object E extends Direction, Dir4, Dir8:
  val vector = Vector2(1, 0)
case object S extends Direction, Dir4, Dir8:
  val vector = Vector2(0, 1)
case object W extends Direction, Dir4, Dir8:
  val vector = Vector2(-1, 0)
case object N extends Direction, Dir4, Dir8:
  val vector = Vector2(0, -1)
case object NE extends Direction, Dir8:
  val vector = Vector2(1, -1)
case object SE extends Direction, Dir8:
  val vector = Vector2(1, 1)
case object SW extends Direction, Dir8:
  val vector = Vector2(-1, 1)
case object NW extends Direction, Dir8:
  val vector = Vector2(-1, -1)

object Direction:
  val all4 = List[Dir4](E, S, W, N)
  val all8 = List[Dir8](E, SE, S, SW, W, NW, N, NE)
