package aoc2021

import scala.annotation.tailrec
import scala.collection.View
import scala.io.Source

def splitChunks(xs: Array[String]): Seq[Seq[String]] =
  @tailrec
  def recurse(rows: List[String], group: Seq[String], all: Seq[Seq[String]]): Seq[Seq[String]] =
    rows match
      case Nil          => all :+ group
      case "" :: tail   => recurse(tail, Seq.empty, all :+ group)
      case head :: tail => recurse(tail, group :+ head, all)
  recurse(xs.toList, Seq.empty, Seq.empty)

extension [A](xs: List[A])
  /** Accumulate a result by comparing pairs of elements across the list. If `stride` is 1 (the default), for example,
    * each step compares item [i] to [i+1]. If `stride` is 2, each step compares item [i] to [i+2], and so on. Only
    * complete pairs are considered.
    */
  def pairwiseFoldLeft[B](z: B)(op: (B, A, A) => B, stride: Int = 1): B =
    assert(stride >= 1, "`stride` must be at least 1.")
    assert(xs.length > stride, "Input list must contain more than `stride` elements.")
    @tailrec
    def recurse(prevs: List[A], currs: List[A], result: B): B =
      if (currs.isEmpty) result
      else
        val a = prevs.head
        val b = currs.head
        recurse(prevs.tail ::: b :: Nil, currs.tail, op(result, a, b))
    recurse(xs.take(stride), xs.drop(stride), z)

def parseInt(x: String, radix: Int): Int   = java.lang.Integer.parseInt(x, radix)
def parseLong(x: String, radix: Int): Long = java.lang.Long.parseLong(x, radix)

def repeatedly[A](f: A => A)(z: A, times: Int): A =
  View.iterate(z, times + 1)(f).last

object Binary:
  extension (x: Boolean)
    /** returns 1 if true, 0 if false */
    def toBinary: Int = if x then 1 else 0

  case class BitSize(n: Int):
    val mask: Int    = (1 << n) - 1
    def range: Range = 0 until n

  def parse(x: String): Int = parseInt(x, 2)

  def invert(number: Int)(using b: BitSize): Int =
    number ^ b.mask

  @tailrec
  def onesCount(number: Int, result: Int = 0)(using b: BitSize): Int =
    if (number == 0) result
    else onesCount(number & (number - 1), result + 1)

  def zerosCount(number: Int)(using b: BitSize): Int =
    b.n - onesCount(number)

  def rotate(number: Int)(using b: BitSize): Int =
    var (n, r) = (number, 0)
    for i <- b.range do
      r = (r << 1) | (n & 1)
      n >>= 1
    r

object Grid2D:
  case class Point(x: Int, y: Int):
    def adjacent4: List[Point] = List(
      Point(x, y - 1),
      Point(x + 1, y),
      Point(x, y + 1),
      Point(x - 1, y)
    )
    def adjacent8: List[Point] = List(
      Point(x, y - 1),
      Point(x + 1, y - 1),
      Point(x + 1, y),
      Point(x + 1, y + 1),
      Point(x, y + 1),
      Point(x - 1, y + 1),
      Point(x - 1, y),
      Point(x - 1, y - 1)
    )

  object Point:
    val zero = Point(0, 0)

  type Grid2D[A] = Map[Point, A]

  def parseGrid[A](f: Char => A)(lines: Array[String]): Grid2D[A] =
    val entries = for
      (ln, y) <- lines.zipWithIndex
      (c, x)  <- ln.zipWithIndex
    yield Point(x, y) -> f(c)
    Map.from(entries)
