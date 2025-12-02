package aoc2021

import scala.annotation.tailrec
import scala.collection.View

import aoc.Day

object Day20 extends Day:
  // NOTE: this problem requires handling a theoretically infinite plane of updates.
  // This is not evident in the example but is for my input because
  // the encoding starts with '#' -- meaning every pixel out on the infinite fringe
  // initially dark, will flip to light on the first expand.
  // The code also ends with '.', so in the next expansion the infinite fringe
  // will all flip back to dark.
  // Thus, we can toggle the default value of unmapped pixels after each step.
  // Thankfully we do this an even number of times, otherwise "how many pixels are lit"
  // would be infinity.
  // All that's left to do is consider the image area plus its two-pixel margin,
  // implying the image area grows in size with each expansion.

  case class Input(code: IndexedSeq[Int], image: Image)

  case class Image(pixels: Set[Vector], defaultValue: Int, xs: Range, ys: Range):
    val setValue                            = defaultValue ^ 1
    def apply(v: Vector): Int               = if pixels.contains(v) then setValue else defaultValue
    def apply(x: Int, y: Int): Int          = this.apply(Vector(x, y))
    def expandRange(n: Int): (Range, Range) = (xs.start - n until xs.end + n, ys.start - n until ys.end + n)

  def toBit(c: Char): Int = c match
    case '#' => 1
    case '.' => 0

  def parse(xs: Array[String]): Input =
    val xs1  = xs.take(1)
    val xs2  = xs.drop(2)
    val code = xs1.head.view.map(toBit).toIndexedSeq
    val pixels = for
      (row, j) <- xs2.view.zipWithIndex
      (col, i) <- row.zipWithIndex
      if col == '#'
    yield Vector(i, j)
    Input(code, Image(pixels.toSet, 0, 0 until xs2(0).size, 0 until xs2.size))

  def expand(input: Input): Input =
    val Input(code, image) = input
    // Each expansion flips which pixel value is explicitly tracked
    // so as to avoid tracking infinite pixels!
    val ndv = input.image.defaultValue ^ 1
    def pixel(x: Int, y: Int): Boolean =
      var n = 0
      for j <- -1 to 1; i <- -1 to 1 do n = (n << 1) | image(i + x, j + y)
      code(n) != ndv

    val (nxs, nys) = image.expandRange(2)
    val pixels     = for j <- nys; i <- nxs; if pixel(i, j) yield Vector(i, j)
    input.copy(image = Image(pixels.toSet, ndv, nxs, nys))

  def draw(input: Input): String =
    val s = new StringBuilder()
    for j <- input.image.ys do
      for i <- input.image.xs do
        val x = input.image(i, j)
        s += (if x == 0 then '.' else '#')
      s ++= "\n"
    s.result()

  def part1(input: Input) =
    val result = View.iterate(input, 3)(expand).last
    result.image.pixels.size

  def part2(input: Input) =
    val result = View.iterate(input, 51)(expand).last
    result.image.pixels.size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
