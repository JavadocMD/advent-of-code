package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day13 extends Day:
  enum Value:
    case N(value: Int)
    case L(xs: List[Value])

    override def toString = this match
      case N(x)  => x.toString
      case L(xs) => s"[${xs.mkString(",")}]"
  import Value._

  type Packet = List[Value]

  def digitsToInt(ds: List[Char]) = ds.foldLeft(0)((n, d) => n * 10 + d - '0')

  def parsePacket(str: String): Packet =
    def recurse(xs: List[Char], acc: Packet): (Packet, List[Char]) =
      xs match
        case Nil => (acc, Nil)
        case '[' :: tail =>
          val (list, tail2) = recurse(tail, Nil)
          recurse(tail2, acc :+ L(list))
        case ']' :: tail => (acc, tail)
        case ',' :: tail => recurse(tail, acc)
        case xs =>
          val (digits, tail) = xs.span(_.isDigit)
          val num            = digitsToInt(digits)
          recurse(tail, acc :+ N(num))
    recurse(str.toList, Nil)._1

  type Input = Seq[Packet]

  def parse(xs: Iterator[String]): Input = xs
    .filter(_ != "")
    .map(parsePacket)
    .toList

  def isOrdered(pa: Packet, pb: Packet): Option[Boolean] = (pa, pb) match
    case (Nil, Nil) => None
    case (Nil, _)   => Some(true)
    case (_, Nil)   => Some(false)
    case (ahead :: atail, bhead :: btail) =>
      (ahead, bhead) match
        case (N(a), N(b)) if a < b => Some(true)
        case (N(a), N(b)) if a > b => Some(false)
        case (N(a), N(b))          => isOrdered(atail, btail)
        case (L(as), L(bs))        => isOrdered(as, bs).orElse(isOrdered(atail, btail))
        case (L(as), b: N)         => isOrdered(as, List(b)).orElse(isOrdered(atail, btail))
        case (a: N, L(bs))         => isOrdered(List(a), bs).orElse(isOrdered(atail, btail))

  def part1(input: Input) = input
    .grouped(2)
    .zipWithIndex
    .filter { case (Seq(a, b), _) => isOrdered(a, b).get }
    .map(_._2 + 1)
    .sum

  given Ordering[Packet] with
    def compare(a: Packet, b: Packet): Int = isOrdered(a, b) match
      case Some(true)  => -1
      case Some(false) => 1
      case None        => 0

  def part2(input: Input) =
    val d1 = parsePacket("[[2]]")
    val d2 = parsePacket("[[6]]")
    val ps = (input :+ d1 :+ d2).toVector.sorted
    val i1 = ps.indexOf(d1) + 1
    val i2 = ps.indexOf(d2) + 1
    i1 * i2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
