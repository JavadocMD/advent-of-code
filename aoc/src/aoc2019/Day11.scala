package aoc2019

import aoc._
import scala.annotation.tailrec

import aoc.Day

object Day11 extends Day:
  import IntcodeComputer as C

  type Input = C.Program
  def parse(xs: Array[String]): Input = C.parseMemory(xs.head)

  case class Vector(x: Int, y: Int):
    def +(that: Vector) = Vector(this.x + that.x, this.y + that.y)
    def -(that: Vector) = Vector(this.x - that.x, this.y - that.y)

  object Vector:
    val zero = Vector(0, 0)

  sealed trait Paint { def value: Long; def symbol: Char }
  case object Black extends Paint { val value = 0; val symbol = '.' }
  case object White extends Paint { val value = 1; val symbol = '#' }

  sealed trait Turn { def value: Long }
  case object Left  extends Turn { val value = 0 }
  case object Right extends Turn { val value = 1 }

  sealed trait Facing {
    def vector: Vector
    def turn(direction: Turn): Facing
  }
  case object North extends Facing {
    val vector                = Vector(0, -1)
    def turn(direction: Turn) = if (direction == Right) East else West
  }
  case object South extends Facing {
    val vector                = Vector(0, +1)
    def turn(direction: Turn) = if (direction == Right) West else East
  }
  case object East extends Facing {
    val vector                = Vector(+1, 0)
    def turn(direction: Turn) = if (direction == Right) South else North
  }
  case object West extends Facing {
    val vector                = Vector(-1, 0)
    def turn(direction: Turn) = if (direction == Right) North else South
  }

  case class Painter(position: Vector, facing: Facing, painted: Map[Vector, Paint]) {
    def move(paint: Paint, turn: Turn): Painter = {
      val nextFacing = facing.turn(turn)
      Painter(position + nextFacing.vector, nextFacing, painted.updated(position, paint))
    }

    val currentPaint = painted.getOrElse(position, Black)
  }
  object Painter {
    val blank        = Painter(Vector.zero, North, Map())
    val startOnWhite = blank.copy(painted = Map(Vector.zero -> White))
  }

  def runPaintProgram(prg: IndexedSeq[Long], init: Painter = Painter.blank): Painter = {
    import IntcodeComputer.{State, step}

    def input(p: Painter) = Iterator.continually(p.currentPaint.value)

    /** * Run the program until complete or we have two outputs.
      */
    @tailrec
    def run(s: State): State = {
      if (s.isComplete) s
      else if (s.output.length == 2) s
      else run(step(s))
    }

    /** * Run the painter until complete.
      */
    @tailrec
    def cycle(p0: Painter, s0: State): Painter = {
      run(s0) match {
        case s if s.isComplete => p0
        case s @ State(_, _, out0 :: out1 :: Nil, _, _) =>
          val paint = if (out0 == 0) Black else White
          val turn  = if (out1 == 0) Left else Right
          val p1    = p0.move(paint, turn)
          val s1 = s.copy(
            input = input(p1),
            output = Seq()
          )
          cycle(p1, s1)
        case x => throw new Exception(s"Unexpected program state: $x")
      }
    }

    cycle(init, State(prg, input(init)))
  }

  def minMax(xs: Iterable[Int]): (Int, Int) = {
    val x0 = xs.head
    xs.tail.foldLeft((x0, x0)) {
      case ((min, max), curr) if curr < min => (curr, max)
      case ((min, max), curr) if curr > max => (min, curr)
      case (minMax, _)                      => minMax
    }
  }

  def draw(p: Painter): String = {
    val (xMin, xMax) = minMax(p.painted.keys.map(_.x))
    val (yMin, yMax) = minMax(p.painted.keys.map(_.y))
    var s            = ""
    for { j <- yMin to yMax } {
      for { i <- xMin to xMax } {
        s += p.painted.getOrElse(Vector(i, j), Black).symbol
      }
      s += "\n"
    }
    s.trim
  }

  def part1(input: Input): Long =
    val res0 = runPaintProgram(input)
    res0.painted.size

  def part2(input: Input): String =
    val res1 = runPaintProgram(input, Painter.startOnWhite)
    draw(res1)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
