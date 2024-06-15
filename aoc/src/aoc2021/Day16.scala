package aoc2021

import scala.annotation.tailrec

import aoc.Day

object Day16 extends Day:
  type Input = List[Int]

  def parse(xs: Array[String]): Input =
    val hexbits =
      for c <- xs.head.toList
      yield c match
        case '0' => List(0, 0, 0, 0)
        case '1' => List(0, 0, 0, 1)
        case '2' => List(0, 0, 1, 0)
        case '3' => List(0, 0, 1, 1)
        case '4' => List(0, 1, 0, 0)
        case '5' => List(0, 1, 0, 1)
        case '6' => List(0, 1, 1, 0)
        case '7' => List(0, 1, 1, 1)
        case '8' => List(1, 0, 0, 0)
        case '9' => List(1, 0, 0, 1)
        case 'A' => List(1, 0, 1, 0)
        case 'B' => List(1, 0, 1, 1)
        case 'C' => List(1, 1, 0, 0)
        case 'D' => List(1, 1, 0, 1)
        case 'E' => List(1, 1, 1, 0)
        case 'F' => List(1, 1, 1, 1)
    hexbits.flatten

  extension [A](i: Iterator[A])
    def nextToList(n: Int): List[A] = List.fill(n)(i.next)
    def nextInt(n: Int): Int        = parseInt(i.nextToList(n).mkString, 2)

  enum Packet(version: Int, id: Int):
    case Literal(version: Int, id: Int, value: Long)         extends Packet(version, id)
    case Operator(version: Int, id: Int, subs: List[Packet]) extends Packet(version, id)
  import Packet._

  def readLiteral(version: Int, id: Int, i: Iterator[Int], value: List[Int] = List.empty): (Literal, Int) =
    val more       = i.next
    val chunkValue = i.nextToList(4)
    if more == 0 then
      val finalValue = value ::: chunkValue
      val total      = finalValue.size / 4 + finalValue.size
      (Literal(version, id, parseLong(finalValue.mkString, 2)), total)
    else readLiteral(version, id, i, value ::: chunkValue)

  def readOperator(version: Int, id: Int, i: Iterator[Int]): (Operator, Int) =
    var total = 0
    var subs  = Seq.empty[Packet]
    i.next match
      case 0 =>
        val len = i.nextInt(15)
        while total < len do
          val (p, n) = readPacket(i)
          total += n
          subs :+= p
        total += 15
      case 1 =>
        val totalPackets = i.nextInt(11)
        var packets      = 0
        while packets < totalPackets do
          val (p, n) = readPacket(i)
          total += n
          packets += 1
          subs :+= p
        total += 11
    (Operator(version, id, subs.toList), total + 1)

  def readPacket(i: Iterator[Int]): (Packet, Int) =
    val version = i.nextInt(3)
    val id      = i.nextInt(3)
    val (p, n) = id match
      case 4 => readLiteral(version, id, i)
      case _ => readOperator(version, id, i)
    (p, n + 6)

  def versionSum(p: Packet): Int =
    p match
      case p: Literal  => p.version
      case p: Operator => p.version + p.subs.map(versionSum).sum

  def part1(input: Input) =
    val (p, _) = readPacket(input.iterator)
    versionSum(p)

  def eval(p: Packet): Long = p match
    case Literal(_, _, value) => value
    case Operator(_, id, subs) =>
      val operands = subs.map(eval)
      id match
        case 0 => operands.sum
        case 1 => operands.product
        case 2 => operands.min
        case 3 => operands.max
        case 5 => if operands(0) > operands(1) then 1 else 0
        case 6 => if operands(0) < operands(1) then 1 else 0
        case 7 => if operands(0) == operands(1) then 1 else 0

  def part2(input: Input) =
    val (p, _) = readPacket(input.iterator)
    eval(p)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
