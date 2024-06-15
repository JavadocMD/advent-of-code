package aoc2022

import scala.annotation.tailrec
import scala.collection.mutable.Stack

import aoc.Day

object Day05 extends Day:
  type Crates = Vector[Stack[Char]]
  case class Move(n: Int, from: Int, to: Int)
  case class Input(crates: Crates, moves: List[Move])

  def parseCrates(xs: Array[String]): Crates =
    val crates = Vector.fill(9)(Stack.empty[Char])
    xs.foreach { s =>
      s.grouped(4).map(_.take(3)).zipWithIndex.foreach {
        case (s"[$c]", i) => crates(i).push(c.head)
        case ("   ", _)   => // no crate
        case (x, _)       => throw new Exception(s"invalid crate format ($x)")
      }
    }
    crates.map(_.reverse)

  def copyCrates(crates: Crates): Crates = crates.map(Stack.from(_))

  def parseMoves(xs: Array[String]): List[Move] =
    xs.toList.map {
      case s"move $n from $from to $to" => Move(n.toInt, from.toInt - 1, to.toInt - 1)
      case x                            => throw new Exception(s"invalid move format ($x)")
    }

  def parse(xs: Array[String]): Input =
    val i      = xs.indexWhere(_ == "")
    val crates = parseCrates(xs.slice(0, i - 1))
    val moves  = parseMoves(xs.slice(i + 1, xs.length))
    Input(crates, moves)

  def runMoves(input: Input): Crates =
    var crates = copyCrates(input.crates)
    input.moves.foreach { case Move(n, from, to) =>
      for i <- 0 until n do
        val c = crates(from).pop()
        crates(to).push(c)
    }
    crates

  def part1(input: Input) =
    val result = runMoves(input)
    result.map(_.headOption).flatten.mkString

  def runMoves2(input: Input): Crates =
    var crates = copyCrates(input.crates)
    input.moves.foreach { case Move(n, from, to) =>
      val temp = Stack.empty[Char]
      for i <- 0 until n do
        val c = crates(from).pop()
        temp.push(c)
      crates(to).pushAll(temp)
    }
    crates

  def part2(input: Input) =
    val result = runMoves2(input)
    result.map(_.headOption).flatten.mkString

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
