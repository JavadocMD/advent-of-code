package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day07b extends Day:
  type Input = List[String]
  def parse(xs: Iterator[String]): Input = xs.toList

  def part1(input: Input) =
    val dirs = readCommands(input)
    dirs.filter(_.size <= 100000).map(_.size).sum

  def part2(input: Input) =
    val dirs       = readCommands(input)
    val root       = dirs.head
    val usedSize   = root.size
    val unusedSize = 70000000 - usedSize
    val toFreeSize = 30000000 - unusedSize
    dirs.filter(_.size >= toFreeSize).minBy(_.size).size

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))

  type Path = List[String]

  object Path:
    val rootPath: Path = Nil
    def isChildOf(parent: Path)(child: Path): Boolean =
      child.tail == parent
  import Path._

  enum Node:
    case File(val name: String, val size: Int)
    case DirRef(val path: Path)
  import Node._

  case class Dir(val path: Path, val size: Int, contents: List[Node])

  def readCommands(commandLog: List[String]): List[Dir] =
    type DirMap = Map[Path, List[Node]]
    @tailrec
    def discover(remainingLog: List[String], path: Path, dirMap: DirMap): DirMap =
      remainingLog match
        case Nil                   => dirMap
        case s"$$ cd /" :: tail    => discover(tail, Nil, dirMap)
        case s"$$ cd .." :: tail   => discover(tail, path.tail, dirMap)
        case s"$$ cd $dir" :: tail => discover(tail, dir :: path, dirMap)
        case s"$$ ls" :: tail =>
          val (entries, rest) = tail.span(!_.startsWith("$"))
          val contents = entries.map {
            case s"dir $name"   => DirRef(name :: path)
            case s"$size $name" => File(name, size.toInt)
          }
          discover(rest, path, dirMap + (path -> contents))
        case x =>
          throw new Exception(s"unexpected line: $x")
    end discover

    // First use the command log to discover the shape of the file system.
    val dirMap = discover(commandLog, path = rootPath, dirMap = Map.empty)

    def calcSizesFrom(path: Path): List[Dir] =
      val contents    = dirMap(path)
      val filesSize   = contents.collect { case File(_, size) => size }.sum
      val descendants = contents.collect { case DirRef(subpath) => calcSizesFrom(subpath) }.flatten
      // only our files and direct subdirectories contribute to the size of _this_ dir.
      val isChild     = isChildOf(path)
      val subdirsSize = descendants.filter(x => isChild(x.path)).map(_.size).sum
      Dir(path, filesSize + subdirsSize, contents) :: descendants
    end calcSizesFrom

    // Second pass to calculate the sizes of all directories recursively.
    calcSizesFrom(rootPath)
  end readCommands
