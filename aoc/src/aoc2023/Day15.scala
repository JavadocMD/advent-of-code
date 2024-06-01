package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day15 extends Day:

  type Input = List[String]

  def parse(xs: List[String]): Input = xs.head.split(",").toList

  def hash(s: String): Int =
    s.foldLeft(0): (value, char) =>
      ((value + char.toInt) * 17) % 256

  def part1(input: Input) = input.map(hash).sum

  import scala.collection.{mutable as m}

  class Box(val lenses: m.ListBuffer[(String, Int)], val labels: m.HashSet[String]):
    def remove(label: String): Unit =
      if labels(label) then
        val i = lenses.indexWhere(_._1 == label)
        lenses.remove(i)
        labels -= label

    def add(label: String, focal: Int): Unit =
      if labels(label) then
        val i = lenses.indexWhere(_._1 == label)
        lenses.update(i, (label, focal))
      else
        lenses.append((label, focal))
        labels += label

  def score(boxes: Seq[Box]): Long =
    val scores = for
      (b, i)      <- boxes.view.zipWithIndex
      ((_, f), j) <- b.lenses.zipWithIndex
    yield (i + 1) * (j + 1) * f
    scores.sum

  def part2(input: Input) =
    val boxes = IndexedSeq.fill(256)(Box(m.ListBuffer.empty, m.HashSet.empty))

    for line <- input do
      line match
        case s"$label-"       => boxes(hash(label)).remove(label)
        case s"$label=$focal" => boxes(hash(label)).add(label, focal.toInt)

    score(boxes)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
