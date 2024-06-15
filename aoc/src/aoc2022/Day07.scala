package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day07 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  def part1(input: Input) =
    val res0 = readCommands(input.toList)
    val res1 = makeTree(res0, rootDir)
    val res2 = flattenTree(res1 :: Nil)
    res2.filter(_.size <= 100000).map(_.size).sum

  def part2(input: Input) =
    val res0       = readCommands(input.toList)
    val res1       = makeTree(res0, rootDir)
    val res2       = flattenTree(res1 :: Nil)
    val usedSize   = res1.size
    val unusedSize = 70000000 - usedSize
    val freeSize   = 30000000 - unusedSize
    res2.filter(_.size >= freeSize).minBy(_.size).size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))

  val rootDir = "/"

  enum Node(val name: String, val size: Int):
    case File(override val name: String, override val size: Int) extends Node(name, size)
    case Dir(override val name: String, override val size: Int, val path: String, contents: List[Node])
        extends Node(name, size)
  import Node._

  def pathString(path: List[String]): String = "/" + path.reverseIterator.mkString("/")
  def pathName(pathString: String): String = pathString.split("/") match
    case Array() => "/"
    case xs      => xs.last

  @tailrec
  def readCommands(
      lines: List[String],
      path: List[String] = Nil,
      contents: Map[String, List[Node]] = Map.empty
  ): Map[String, List[Node]] =
    lines match
      case Nil => contents
      case s"$$ cd /" :: tail =>
        readCommands(tail, Nil, contents)
      case s"$$ cd .." :: tail =>
        readCommands(tail, path.tail, contents)
      case s"$$ cd $dir" :: tail =>
        readCommands(tail, dir :: path, contents)
      case s"$$ ls" :: tail =>
        val (entries, rest) = tail.span(!_.startsWith("$"))
        val nodes = entries.map {
          case s"dir $name"   => Dir(name, 0, pathString(name :: path), Nil)
          case s"$size $name" => File(name, size.toInt)
        }
        readCommands(rest, path, contents + (pathString(path) -> nodes))
      case x =>
        throw new Exception(s"unexpected line: $x")

  def makeTree(mapping: Map[String, List[Node]], path: String): Dir =
    val contents = mapping(path).map {
      case f: File               => f
      case Dir(name, _, path, _) => makeTree(mapping, path)
    }
    Dir(pathName(path), contents.map(_.size).sum, path, contents)

  @tailrec
  def flattenTree(unexplored: List[Dir], dirs: List[Dir] = Nil): List[Dir] =
    unexplored match
      case Nil => dirs
      case head :: tail =>
        val next = head.contents.collect { case d: Dir => d }
        flattenTree(tail ++ next, head :: dirs)
