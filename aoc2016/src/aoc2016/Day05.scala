package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Util._
import java.security.MessageDigest

object Day05 extends Day:

  lazy val input = loadInput().head

  def prefixMd5(prefix: String): Long => Array[Byte] =
    val md          = MessageDigest.getInstance("MD5")
    val prefixBytes = prefix.getBytes

    def forSuffix(suffix: Long): Array[Byte] =
      md.update(prefixBytes)
      md.update(suffix.toString.getBytes)
      md.digest()
      // warning: Scala treats bytes as signed!
    forSuffix

  val md5Sequence: LazyList[(Int, Array[Byte])] =
    val computeMd5 = prefixMd5(input)
    LazyList.from(
      Iterator
        .from(0)
        .map(i => (i, computeMd5(i)))
        .filter: (i, bs) =>
          // First five words must be zero
          bs(0) == 0 && bs(1) == 0 && (bs(2) & 0xf0) == 0
    )

  lazy val part1 =
    md5Sequence
      .take(8)
      .map: (i, bs) =>
        "%x".format(bs(2))
      .mkString

  lazy val part2 =
    md5Sequence
      .filter: (i, bs) =>
        bs(2) < 0x08 // Now the sixth word must be a valid index (0-7)
      .map: (i, bs) =>
        val pos = bs(2)
        val chr = "%x".format((bs(3) & 0xf0) >>> 4)
        (pos, chr)
      .scanLeft(IndexedSeq.fill(8)("")):
        case (acc, (pos, chr)) if acc(pos) == "" => acc.updated(pos, chr)
        case (acc, (pos, chr))                   => acc
      .find(_.forall(_.nonEmpty))
      .get
      .mkString

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
