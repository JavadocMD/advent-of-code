package aoc2016

import scala.annotation.tailrec
import aoc.Day

object Day09 extends Day:

  lazy val input = loadInput().head

  val marker = """\((\d+)x(\d+)\)""".r.unanchored

  def decompressedSize(s: String, acc: Long = 0L): Long =
    marker.findFirstMatchIn(s) match
      case None => acc + s.size
      case Some(m) =>
        val sectionSize       = m.group(1).toInt
        val sectionTimes      = m.group(2).toInt
        val compressedChars   = sectionSize * sectionTimes
        val uncompressedChars = m.start
        val continueFrom      = m.end + sectionSize
        decompressedSize(s.drop(continueFrom), acc + uncompressedChars + compressedChars)

  lazy val part1 = decompressedSize(input)

  def decompressedSize2(s: String, acc: Long = 0L): Long =
    marker.findFirstMatchIn(s) match
      case None => acc + s.size
      case Some(m) =>
        val sectionSize       = m.group(1).toInt
        val sectionTimes      = m.group(2).toInt
        val compressedChars   = decompressedSize2(s.slice(m.end, m.end + sectionSize)) * sectionTimes
        val uncompressedChars = m.start
        val continueFrom      = m.end + sectionSize
        decompressedSize2(s.drop(continueFrom), acc + uncompressedChars + compressedChars)

  lazy val part2 = decompressedSize2(input)

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
