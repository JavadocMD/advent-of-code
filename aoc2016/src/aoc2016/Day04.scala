package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Util._

object Day04 extends Day:

  case class Room(name: String, id: Int, checksum: List[Char])

  val pattern = """(.+)-(\d+)\[([a-z]+)\]""".r

  lazy val input = loadInput().map:
    case pattern(name, id, checksum) => Room(name, id.toInt, checksum.toList)

  def checksum(name: String): List[Char] =
    val charCounts = name.toList.groupMapReduce(identity)(_ => 1)(_ + _).removed('-')
    charCounts.view.map((c, n) => (-n, c)).toList.sorted.take(5).map(_._2)

  lazy val part1 = input.view
    .filter: x =>
      checksum(x.name) == x.checksum
    .map(_.id)
    .sum

  def decrypt(room: Room): String =
    val shift = room.id % 26
    val mapping = ((shift until 26) ++ (0 until shift)).map: i =>
      ('a' + i).toChar
    room.name
      .map:
        case '-' => ' '
        case x   => mapping(x - 'a')
      .mkString

  lazy val part2 = input.view
    .filter: x =>
      checksum(x.name) == x.checksum
    .find: x =>
      decrypt(x).contains("northpole")
    .map(_.id)
    .get

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
