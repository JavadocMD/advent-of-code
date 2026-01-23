package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Util._

object Day08 extends Day:

  sealed trait Instruction
  case class Rect(w: Int, h: Int)       extends Instruction
  case class RotRow(row: Int, num: Int) extends Instruction
  case class RotCol(col: Int, num: Int) extends Instruction

  lazy val input = loadInput().map:
    case s"rect ${x}x${y}"               => Rect(x.toInt, y.toInt)
    case s"rotate row y=$row by $num"    => RotRow(row.toInt, num.toInt)
    case s"rotate column x=$col by $num" => RotCol(col.toInt, num.toInt)

  type Screen = Array[Array[Boolean]]

  val WIDTH  = 50
  val HEIGHT = 6

  def apply(instr: Instruction, screen: Screen): Screen =
    instr match
      case Rect(w, h) =>
        for
          j <- 0 until h
          i <- 0 until w
        do screen(j)(i) = true
        screen
      case RotRow(row, num) =>
        val prevRow = screen(row).toIndexedSeq
        for i <- 0 until WIDTH do screen(row)(i) = prevRow((i + WIDTH - num) % WIDTH)
        screen
      case RotCol(col, num) =>
        val prevCol = screen.transpose()(col).toIndexedSeq
        for j <- 0 until HEIGHT do screen(j)(col) = prevCol((j + HEIGHT - num) % HEIGHT)
        screen

  lazy val part1 =
    val result = input.foldLeft(Array.fill(HEIGHT, WIDTH)(false)):
      case (acc, instr) => apply(instr, acc)
    result.map(_.count(_ == true)).sum

  lazy val part2 =
    val result = input.foldLeft(Array.fill(HEIGHT, WIDTH)(false)):
      case (acc, instr) => apply(instr, acc)
    result
      .map: row =>
        row.map(if _ then '#' else ' ').mkString
      .mkString("\n")

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
