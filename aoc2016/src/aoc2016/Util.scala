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

  sealed trait Direction:
    val vector: Vector2
  case object E extends Direction:
    val vector = Vector2(1, 0)
  case object S extends Direction:
    val vector = Vector2(0, 1)
  case object W extends Direction:
    val vector = Vector2(-1, 0)
  case object N extends Direction:
    val vector = Vector2(0, -1)
  case object NE extends Direction:
    val vector = Vector2(1, -1)
  case object SE extends Direction:
    val vector = Vector2(1, 1)
  case object SW extends Direction:
    val vector = Vector2(-1, 1)
  case object NW extends Direction:
    val vector = Vector2(-1, -1)

  object Direction:
    val all4 = List(E, S, W, N)
    val all8 = List(E, SE, S, SW, W, NW, N, NE)

  sealed trait Turn
  case object Left  extends Turn
  case object Right extends Turn

  extension (dir: Direction)
    def turn90(turn: Turn): Direction = (dir, turn) match
      case (E, Left)  => N
      case (E, Right) => S
      case (S, Left)  => E
      case (S, Right) => W
      case (W, Left)  => S
      case (W, Right) => N
      case (N, Left)  => W
      case (N, Right) => E
      case _          => throw new Exception("turn90 only supports N,E,S,W directions")

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
