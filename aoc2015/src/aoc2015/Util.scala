package aoc2015

import scala.annotation.tailrec

object Util:
  case class Vector2(x: Int, y: Int) extends Ordered[Vector2]:
    def +(that: Vector2): Vector2     = Vector2(this.x + that.x, this.y + that.y)
    def -(that: Vector2): Vector2     = Vector2(this.x - that.x, this.y - that.y)
    def *(that: Vector2): Vector2     = Vector2(this.x * that.y, this.y * that.y)
    def /(that: Vector2): Vector2     = Vector2(this.x / that.y, this.y / that.y)
    def neighbors4: List[Vector2]     = Direction.all4.map(this + _)
    def neighbors8: List[Vector2]     = Direction.all8.map(this + _)
    def manhattan(that: Vector2): Int = (that.x - this.x).abs + (that.y - this.y).abs
    override def toString(): String   = s"$x,$y"

    def compare(that: Vector2): Int =
      val dx = this.x.compareTo(that.x)
      if dx != 0 then dx else this.y.compareTo(that.y)

  object Vector2:
    def s(x: String, y: String): Vector2 = Vector2(x.toInt, y.toInt)

  object Direction:
    val E    = Vector2(1, 0)
    val S    = Vector2(0, 1)
    val W    = Vector2(-1, 0)
    val N    = Vector2(0, -1)
    val all4 = List(E, S, W, N)
    val NE   = Vector2(1, -1)
    val SE   = Vector2(1, 1)
    val SW   = Vector2(-1, 1)
    val NW   = Vector2(-1, -1)
    val all8 = List(E, SE, S, SW, W, NW, N, NE)

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
