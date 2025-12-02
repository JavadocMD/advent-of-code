package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day08 extends Day:
  type Input = Vector[Vector[Int]]
  def parse(xs: Iterator[String]): Input = xs.map(_.map(_.toString.toInt).toVector).toVector

  def part1(input: Input) =
    val inputTrx = input.transpose
    val width    = input(0).size
    val height   = input.size

    val rangeW = 1 until (width - 1)
    val rangeH = 1 until (height - 1)

    val visibility = Array.fill(width, height)(0L)

    // sweep down
    var sweep = input.head.toArray
    for
      j <- rangeH
      row = input(j)
      i <- rangeW
    do
      if sweep(i) < row(i) then
        sweep(i) = row(i)
        visibility(j)(i) += 1
    // sweep up
    sweep = input.last.toArray
    for
      j <- rangeH.reverse
      row = input(j)
      i <- rangeW.reverse
    do
      if sweep(i) < row(i) then
        sweep(i) = row(i)
        visibility(j)(i) += 1
    // sweep right
    sweep = inputTrx.head.toArray
    for
      i <- rangeW
      col = inputTrx(i)
      j <- rangeH
    do
      if sweep(j) < col(j) then
        sweep(j) = col(j)
        visibility(j)(i) += 1
    // sweep left
    sweep = inputTrx.last.toArray
    for
      i <- rangeW.reverse
      col = inputTrx(i)
      j <- rangeH.reverse
    do
      if sweep(j) < col(j) then
        sweep(j) = col(j)
        visibility(j)(i) += 1

    // println(visibility.map(_.mkString).mkString("\n"))
    val interiorVisible = visibility.map { _.count(_ > 0) }.sum
    val edgeVisible     = width * 2 + height * 2 - 4
    interiorVisible + edgeVisible

  def part2(input: Input) =
    val inputTrx = input.transpose
    val width    = input(0).size
    val height   = input.size

    val score = Array.fill(width, height)(0)

    for
      j <- 0 until height
      row = input(j)
      i <- 0 until width
    do
      val curr    = row(i)
      var k       = i + 1
      var visible = 0
      var blocked = false
      while k < width && !blocked do
        blocked = row(k) >= curr
        visible += 1
        k += 1
      score(j)(i) = visible

    for
      j <- 0 until height
      row = input(j).reverse
      i <- 0 until width
    do
      val curr    = row(i)
      var k       = i + 1
      var visible = 0
      var blocked = false
      while k < width && !blocked do
        blocked = row(k) >= curr
        visible += 1
        k += 1
      score(j)(width - i - 1) *= visible

    for
      i <- 0 until width
      col = inputTrx(i)
      j <- 0 until height
    do
      val curr    = col(j)
      var k       = j + 1
      var visible = 0
      var blocked = false
      while k < width && !blocked do
        blocked = col(k) >= curr
        visible += 1
        k += 1
      score(j)(i) *= visible

    for
      i <- 0 until width
      col = inputTrx(i).reverse
      j <- 0 until height
    do
      val curr    = col(j)
      var k       = j + 1
      var visible = 0
      var blocked = false
      while k < width && !blocked do
        blocked = col(k) >= curr
        visible += 1
        k += 1
      score(height - j - 1)(i) *= visible

    // println(score.map(_.mkString).mkString("\n"))
    score.map(_.max).max

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
