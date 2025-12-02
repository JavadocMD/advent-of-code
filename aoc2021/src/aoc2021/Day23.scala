package aoc2021

import scala.annotation.tailrec
import scala.collection.{mutable => m}

import aoc.Day

object Day23 extends Day:
  enum Amphipod:
    case Empty
    case Amber
    case Bronze
    case Copper
    case Desert
  import Amphipod._

  val cost: Map[Amphipod, Int] = Map(
    Amber  -> 1,
    Bronze -> 10,
    Copper -> 100,
    Desert -> 1000
  )

  enum Kind:
    case NoStop
    case Hall
    case Room
  import Kind._

  type Space = Int

  case class Move(from: Space, to: Space, dist: Int, cost: Int)

  type Board = IndexedSeq[Amphipod]

  object Board:
    def move(b: Board, m: Move): Board =
      b.updated(m.to, b(m.from)).updated(m.from, Empty)

  trait Part:
    def kind: IndexedSeq[Kind]
    def edges: IndexedSeq[List[Space]]
    def room: Map[Amphipod, Seq[Space]]
    def input: Board
    def success: Board
    val hweight: Int

    def movesForPod(board: Board, pod: Amphipod, start: Space): List[Move] =
      val homes     = room(pod)
      val startKind = kind(start)
      def canStop(at: Space): Boolean =
        val atKind = kind(at)
        // can move from any room to any hall
        if startKind == Room && atKind == Hall then true
        // room must be ours, have no different pods in it, and we prefer last open spot
        else if atKind == Room && homes.contains(at) then homes.forall(s => s <= at || board(s) == pod)
        // no other moves permitted
        else false

      var moves = List.empty[Move]
      var seen  = Set.empty[Space]
      var open  = List(Move(start, start, 0, 0))
      while open.nonEmpty do
        val m @ Move(_, s, d, c) = open.head
        open = open.tail
        seen += s
        if (canStop(s)) moves ::= m
        for
          e <- edges(s)
          if !seen.contains(e) && board(e) == Empty
        do open ::= Move(start, e, d + 1, c + cost(pod))
      moves

    def bestSolution: Int =
      def h(board: Board): Int =
        var estimate = 0
        for (pod, spaces) <- room; s <- spaces do
          if (board(s) != pod)
            estimate += cost(pod) * hweight
        estimate

      given Ordering[(Board, Int)] with
        def compare(a: (Board, Int), b: (Board, Int)): Int = b._2.compare(a._2)

      val frontier = m.PriorityQueue((input, 0))
      val score    = m.Map(input -> 0)

      while frontier.nonEmpty do
        val (curr, cost) = frontier.dequeue()
        if (curr == success) frontier.clear()
        else
          for
            (pod, i) <- curr.view.zipWithIndex if pod != Empty
            move     <- movesForPod(curr, pod, i)
          do
            val b = Board.move(curr, move)
            val s = score(curr) + move.cost
            if s < score.getOrElse(b, Int.MaxValue) then
              score(b) = s
              frontier += ((b, s + h(b)))

      score(success)
  end Part

  object Part1 extends Part:
    val hweight: Int = 4

    val kind: IndexedSeq[Kind] = IndexedSeq(
      Hall,
      Hall,
      NoStop,
      Hall,
      NoStop,
      Hall,
      NoStop,
      Hall,
      NoStop,
      Hall,
      Hall,
      Room,
      Room,
      Room,
      Room,
      Room,
      Room,
      Room,
      Room
    )

    val edges: IndexedSeq[List[Space]] = IndexedSeq(
      /* 00 */ List(1),
      /* 01 */ List(0, 2),
      /* 02 */ List(1, 3, 11),
      /* 03 */ List(2, 4),
      /* 04 */ List(3, 5, 13),
      /* 05 */ List(4, 6),
      /* 06 */ List(5, 7, 15),
      /* 07 */ List(6, 8),
      /* 08 */ List(7, 9, 17),
      /* 09 */ List(8, 10),
      /* 10 */ List(9),
      /* 11 */ List(2, 12),
      /* 12 */ List(11),
      /* 13 */ List(4, 14),
      /* 14 */ List(13),
      /* 15 */ List(6, 16),
      /* 16 */ List(15),
      /* 17 */ List(8, 18),
      /* 18 */ List(17)
    )

    val room: Map[Amphipod, Seq[Space]] = Map(
      Amber  -> Seq(11, 12),
      Bronze -> Seq(13, 14),
      Copper -> Seq(15, 16),
      Desert -> Seq(17, 18)
    )

    val input = IndexedSeq.fill(11)(Empty) ++ IndexedSeq(
      Bronze,
      Bronze,
      Amber,
      Copper,
      Amber,
      Desert,
      Desert,
      Copper
    )

    val success = IndexedSeq.fill(11)(Empty) ++
      IndexedSeq.fill(2)(Amber) ++
      IndexedSeq.fill(2)(Bronze) ++
      IndexedSeq.fill(2)(Copper) ++
      IndexedSeq.fill(2)(Desert)
  end Part1

  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  def part1(input: Input) = Part1.bestSolution

  object Part2 extends Part:
    val hweight: Int = 50

    val kind: IndexedSeq[Kind] = IndexedSeq(
      Hall,
      Hall,
      NoStop,
      Hall,
      NoStop,
      Hall,
      NoStop,
      Hall,
      NoStop,
      Hall,
      Hall
    ) ++ IndexedSeq.fill(16)(Room)

    val edges: IndexedSeq[List[Space]] = IndexedSeq(
      /* 00 */ List(1),
      /* 01 */ List(0, 2),
      /* 02 */ List(1, 3, 11),
      /* 03 */ List(2, 4),
      /* 04 */ List(3, 5, 15),
      /* 05 */ List(4, 6),
      /* 06 */ List(5, 7, 19),
      /* 07 */ List(6, 8),
      /* 08 */ List(7, 9, 23),
      /* 09 */ List(8, 10),
      /* 10 */ List(9),
      /* 11 */ List(2, 12),
      /* 12 */ List(11, 13),
      /* 13 */ List(12, 14),
      /* 14 */ List(13),
      /* 15 */ List(4, 16),
      /* 16 */ List(15, 17),
      /* 17 */ List(16, 18),
      /* 18 */ List(17),
      /* 19 */ List(6, 20),
      /* 20 */ List(19, 21),
      /* 21 */ List(20, 22),
      /* 22 */ List(21),
      /* 23 */ List(8, 24),
      /* 24 */ List(23, 25),
      /* 25 */ List(24, 26),
      /* 26 */ List(25)
    )

    val room: Map[Amphipod, Seq[Space]] = Map(
      Amber  -> Seq(11, 12, 13, 14),
      Bronze -> Seq(15, 16, 17, 18),
      Copper -> Seq(19, 20, 21, 22),
      Desert -> Seq(23, 24, 25, 26)
    )

    val input = IndexedSeq.fill(11)(Empty) ++ IndexedSeq(
      Bronze,
      Desert,
      Desert,
      Bronze,
      Amber,
      Copper,
      Bronze,
      Copper,
      Amber,
      Bronze,
      Amber,
      Desert,
      Desert,
      Amber,
      Copper,
      Copper
    )

    val success =
      IndexedSeq.fill(11)(Empty) ++
        IndexedSeq.fill(4)(Amber) ++
        IndexedSeq.fill(4)(Bronze) ++
        IndexedSeq.fill(4)(Copper) ++
        IndexedSeq.fill(4)(Desert)
  end Part2

  def part2(input: Input) = Part2.bestSolution

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
