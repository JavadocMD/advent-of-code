package aoc2019

import aoc._
import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day08 extends Day:
  type Layer = Array[Int]
  case class Data(layers: Array[Layer], size: Size)
  case class Image(layer: Layer, size: Size)

  type Input = Data
  def parse(xs: Array[String], size: Size): Input =
    if (xs.head.size % size.area != 0) throw new Exception("Data load failure: input of unexpected length.")
    val layers = xs.head.view
      .grouped(size.area)
      .map(_.map(_.toString.toInt).toArray)
      .toArray
    Data(layers, size)

  case class Size(width: Int, height: Int):
    val area = width * height

  implicit class IntToSize(width: Int):
    def x(height: Int): Size = Size(width, height)

  def digitCount(layer: Layer, digit: Int): Int = layer.count(_ == digit)

  def checksum(data: Data): Int =
    val layer = data.layers.minBy(digitCount(_, 0))
    digitCount(layer, 1) * digitCount(layer, 2)

  def mergeLayers(front: Layer, back: Layer): Layer =
    front.zip(back).map {
      case (2, x) => x
      case (x, _) => x
    }

  def merge(data: Data): Image =
    val result = data.layers.reduce(mergeLayers)
    Image(result, data.size)

  def display(image: Image): String =
    image.layer
      .map({
        case 0 => " "
        case 1 => "X"
        case x => "."
      })
      .grouped(image.size.width)
      .map(_.mkString)
      .mkString("\n")

  def part1(input: Input): Long = checksum(input)

  def part2(input: Input): String = display(merge(input))

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput(), 25 x 6)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
