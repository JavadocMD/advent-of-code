package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day09 extends Day:

  type Input = List[Long]

  sealed trait Block

  case class File(start: Long, size: Long, id: Long) extends Block:
    def checksum: Long =
      val a = start
      val b = start + size - 1
      id * ((b - a + 1) * (a + b) / 2)

  case class Empty(start: Long, size: Long) extends Block

  def parse(xs: List[String]): Input = xs.head.map(_.toString.toLong).toList

  def readBlocks(input: Input): List[Block] =
    val blockSizes  = if input.size % 2 == 0 then input else input :+ 0L
    val blockStarts = blockSizes.scanLeft(0L)(_ + _).dropRight(1)
    val fileIds     = List.range(0, blockSizes.size).map(_ / 2)

    blockStarts
      .lazyZip(blockSizes)
      .lazyZip(fileIds)
      .toList // now a List[(start, size, fileId)]
      .grouped(2)
      .flatMap:
        case (fstart, fsize, fid) :: (_, 0, _) :: Nil => // drop 0-length Empty blocks
          File(fstart, fsize, fid) :: Nil
        case (fstart, fsize, fid) :: (estart, esize, _) :: Nil =>
          File(fstart, fsize, fid) :: Empty(estart, esize) :: Nil
        case xs => throw Exception(s"Unexpected input: $xs")
      .toList

  def decompressChecksum(blocks: List[Block]): Long =
    @tailrec
    def recurse(queue: Vector[Block], acc: Long): Long =
      if queue.isEmpty then acc
      else
        (queue.head, queue.last) match
          case (_, _: Empty) => recurse(queue.init, acc)
          case (f: File, _)  => recurse(queue.tail, f.checksum + acc)
          case (e: Empty, f: File) =>
            if e.size == f.size then
              val f2 = f.copy(start = e.start)
              recurse(queue.tail.init, f2.checksum + acc)
            else if e.size > f.size then
              val f2 = f.copy(start = e.start)
              val e2 = Empty(e.start + f.size, e.size - f.size)
              recurse(e2 +: queue.tail.init, f2.checksum + acc)
            else // e.size < f.size
              val f2 = f.copy(start = e.start, size = e.size)
              val f3 = f.copy(size = f.size - e.size)
              recurse(queue.tail.init :+ f3, f2.checksum + acc)
    recurse(blocks.toVector, 0)

  def part1(input: Input) =
    val blocks = readBlocks(input)
    decompressChecksum(blocks)

  def decompressChecksum2(blocks: List[Block]): Long =
    @tailrec
    def recurse(files: List[File], blanks: List[Empty], acc: List[File]): List[File] =
      if files.isEmpty then acc
      else
        val f = files.head
        val i = blanks.indexWhere(b => b.size >= f.size && b.start < f.start)
        if i == -1 then recurse(files.tail, blanks, f :: acc)
        else
          val b = blanks(i)
          val blankPatch =
            if f.size == b.size then Nil
            else Empty(b.start + f.size, b.size - f.size) :: Nil
          recurse(files.tail, blanks.patch(i, blankPatch, 1), f.copy(start = b.start) :: acc)

    val files  = blocks.collect { case x: File => x }
    var blanks = blocks.collect { case x: Empty => x }
    recurse(files.reverse, blanks, Nil).map(_.checksum).sum

  def part2(input: Input) =
    val blocks = readBlocks(input)
    decompressChecksum2(blocks)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
