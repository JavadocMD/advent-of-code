package aoc2016

import scala.annotation.tailrec

object Util:
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

  case class Rectangle(topLeft: Vector2, bottomRight: Vector2):
    def contains(point: Vector2): Boolean =
      topLeft.x <= point.x && point.x <= bottomRight.x
        && topLeft.y <= point.y && point.y <= bottomRight.y

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

  sealed trait Turn
  case object Left  extends Turn
  case object Right extends Turn

  extension (dir: Dir4)
    def turn90(turn: Turn): Dir4 = (dir, turn) match
      case (E, Left)  => N
      case (E, Right) => S
      case (S, Left)  => E
      case (S, Right) => W
      case (W, Left)  => S
      case (W, Right) => N
      case (N, Left)  => W
      case (N, Right) => E

  def memoize[A, B](fn: A => B): A => B =
    import scala.collection.{mutable as m}
    val memory = m.Map.empty[A, B]

    def memoized(a: A): B = memory.getOrElseUpdate(a, fn(a))
    memoized

  extension [T](it: Iterable[T]) //
    def every(step: Int): Iterator[T] =
      it.iterator.zipWithIndex
        .filter(_._2 % step == 0)
        .map(_._1)

  extension [T](it: Iterator[T]) //
    def lastOption: Option[T] =
      var curr = Option.empty[T]
      while it.hasNext do curr = Some(it.next)
      curr

  def loopPairs[T](xs: List[T]): Iterator[(T, T)] =
    val a = LazyList.from(xs)
    val b = LazyList.from(xs.tail) :+ xs.head
    (a lazyZip b).iterator

  def loopTriplets[T](xs: List[T]): Iterator[(T, T, T)] =
    val a = LazyList.from(xs)
    val b = LazyList.from(xs.tail) :+ xs.head
    val c = LazyList.from(xs.tail.tail) :+ xs.head :+ xs.tail.head
    (a lazyZip b lazyZip c).iterator

  extension (s: String) //
    def println: Unit = Predef.println(s)
