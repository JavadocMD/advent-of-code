package aoc2021

import scala.annotation.tailrec

import aoc.Day

object Day13 extends Day:
  import Grid2D.Point
  case class Input(dots: Set[Point], folds: List[String])

  def parse(xs: Array[String]): Input =
    val (dots, folds) = xs.view.span(_.nonEmpty)
    Input(
      dots.map({ case s"$x,$y" => Point(x.toInt, y.toInt) }).toSet,
      folds.drop(1).toList
    )

  def foldX(dots: Set[Point], fx: Int): Set[Point] =
    dots.map {
      case p if p.x < fx => p
      case p if p.x > fx => Point(fx - (p.x - fx), p.y)
    }

  def foldY(dots: Set[Point], fy: Int): Set[Point] =
    dots.map {
      case p if p.y < fy => p
      case p if p.y > fy => Point(p.x, fy - (p.y - fy))
    }

  def part1(input: Input) = foldX(input.dots, 655).size

  def dotsToString(dots: Set[Point]): String =
    val w    = dots.maxBy(_.x).x + 1
    val h    = dots.maxBy(_.y).y + 1
    val grid = Array.fill(h, w)('.')
    for p <- dots do grid(p.y)(p.x) = '#'
    grid.map(_.mkString).mkString("\n")

  def part2(input: Input) =
    val dots = input.folds.foldLeft(input.dots) { (dots, fold) =>
      fold match
        case s"fold along x=$fx" => foldX(dots, fx.toInt)
        case s"fold along y=$fy" => foldY(dots, fy.toInt)
    }
    dotsToString(dots)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
