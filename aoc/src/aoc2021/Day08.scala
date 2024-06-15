package aoc2021

import scala.annotation.tailrec

import aoc.Day

object Day08 extends Day:
  type Input    = Seq[(Seq[Int], Seq[Int])]
  type Encoding = Map[Int, Int]

  given Binary.BitSize(7)
  def b(n: Int) = Binary.onesCount(n)

  def parseByte(s: String): Int =
    s.foldLeft(0)((b, c) =>
      c match
        case 'a' => b | 1 << 0
        case 'b' => b | 1 << 1
        case 'c' => b | 1 << 2
        case 'd' => b | 1 << 3
        case 'e' => b | 1 << 4
        case 'f' => b | 1 << 5
        case 'g' => b | 1 << 6
    )

  def parse(input: Array[String]): Input =
    for line <- input.toSeq yield
      val halves = line.split("\\s\\|\\s")
      val in     = halves(0).split("\\s").toSeq.map(parseByte)
      val out    = halves(1).split("\\s").toSeq.map(parseByte)
      (in, out)

  def is1478(n: Int) = b(n) match
    case 2 => true // 1
    case 4 => true // 4
    case 3 => true // 7
    case 7 => true // 8
    case _ => false

  def part1(input: Input) =
    input
      .map({ case (_, out) =>
        out.count(is1478)
      })
      .sum

  def signature(index: (Int, Int, Int, Int), test: Int): (Int, Int, Int, Int) =
    val (i0, i1, i2, i3) = index
    (b(test & i0), b(test & i1), b(test & i2), b(test & i3))

  def decypher(in: Seq[Int]): Encoding =
    val one = in.find(n => b(n) == 2).get
    val fou = in.find(n => b(n) == 4).get
    val sev = in.find(n => b(n) == 3).get
    val eig = in.find(n => b(n) == 7).get

    val index = (one, fou, sev, eig)

    val decyphered = for
      n <- in
      size = b(n)
      if size == 5 || size == 6
    yield
      val result = signature(index, n) match
        case (1, 2, 2, 5) => 2
        case (2, 3, 3, 5) => 3
        case (1, 3, 2, 5) => 5
        case (2, 3, 3, 6) => 0
        case (1, 3, 2, 6) => 6
        case (2, 4, 3, 6) => 9
        case x            => throw new Exception(s"unmatched signature $x")
      (n -> result)

    Map.from(decyphered) ++ Map(
      one -> 1,
      fou -> 4,
      sev -> 7,
      eig -> 8
    )

  def decode(enc: Encoding, out: Seq[Int]): Int =
    out.foldLeft(0) { case (acc, n) =>
      (acc * 10) + enc(n)
    }

  def part2(input: Input) =
    input
      .map({ case (in, out) =>
        decode(decypher(in), out)
      })
      .sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
