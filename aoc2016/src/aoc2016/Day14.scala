package aoc2016

import scala.annotation.tailrec
import aoc.Day
import java.security.MessageDigest
import aoc2016.Util.memoize

object Day14 extends Day:

  lazy val input = loadInput().head

  val hexByte = "0123456789abcdef".getBytes

  // This is an optimization:
  // converting a digest result from bytes to string and back to bytes
  // just so we can hash it again 2016 times is too slow.
  // So instead cut out the middle man and convert directly to hex-equivalent bytes.
  def copyAsHexBytes(in: Array[Byte], out: Array[Byte]): Unit =
    // assume "in" is 16 bytes and "out" is 32 bytes
    var i = 0
    var j = 0
    while i < 16 do
      val b = in(i)
      out(j) = hexByte((b >> 4) & 0x0f)
      out(j + 1) = hexByte(b & 0x0f)
      i += 1
      j += 2

  def prefixMd5(prefix: String): Long => String =
    val md          = MessageDigest.getInstance("MD5")
    val prefixBytes = prefix.getBytes

    def forSuffix(suffix: Long): String =
      md.update(prefixBytes)
      md.update(suffix.toString.getBytes)
      var digBytes = md.digest()
      var hexBytes = Array.ofDim[Byte](32)
      copyAsHexBytes(digBytes, hexBytes)
      new String(hexBytes)
    forSuffix

  val triples    = """(.)\1\1""".r.unanchored
  val quintuples = """(.)\1\1\1\1""".r.unanchored
  case class Md5Info(index: Int, string: String, tris: Set[Char], qnts: Set[Char])

  def md5Sequence(computeMd5: Long => String): LazyList[Md5Info] =
    LazyList.from(
      Iterator
        .from(0)
        .map: i =>
          val md5str = computeMd5(i)
          val tris   = triples.findFirstIn(md5str).map(_.head).toSet
          val qnts   = quintuples.findAllIn(md5str).map(_.head).toSet
          Md5Info(i, md5str, tris, qnts)
    )

  lazy val part1 =
    md5Sequence(prefixMd5(input))
      .sliding(1001)
      .filter:
        case head #:: tail => head.tris.exists(c => tail.exists(_.qnts.contains(c)))
      .map(_.head)
      .drop(64 - 1)
      .next()
      .index

  def prefixMd5Stretched(prefix: String): Long => String =
    val md          = MessageDigest.getInstance("MD5")
    val prefixBytes = prefix.getBytes

    def forSuffix(suffix: Long): String =
      md.update(prefixBytes)
      md.update(suffix.toString.getBytes)
      var digBytes = md.digest()
      var hexBytes = Array.ofDim[Byte](32)
      copyAsHexBytes(digBytes, hexBytes)
      for _ <- 0 until 2016 do
        digBytes = md.digest(hexBytes)
        copyAsHexBytes(digBytes, hexBytes)
      new String(hexBytes)

    forSuffix

  lazy val part2 =
    md5Sequence(prefixMd5Stretched(input))
      .sliding(1001)
      .filter:
        case head #:: tail => head.tris.exists(c => tail.exists(_.qnts.contains(c)))
      .map(_.head)
      .drop(64 - 1)
      .next()
      .index

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
