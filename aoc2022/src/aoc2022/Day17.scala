package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day17 extends Day:
  import Grid2D.Vector2
  val down = Vector2(0, -1)

  enum Jet(val vector: Vector2):
    case Left  extends Jet(Vector2(-1, 0))
    case Right extends Jet(Vector2(1, 0))

  type Rock = List[Vector2]

  object Rock:
    val flat   = List(Vector2(0, 0), Vector2(1, 0), Vector2(2, 0), Vector2(3, 0))
    val plus   = List(Vector2(1, 0), Vector2(0, 1), Vector2(1, 1), Vector2(2, 1), Vector2(1, 2))
    val el     = List(Vector2(0, 0), Vector2(1, 0), Vector2(2, 0), Vector2(2, 1), Vector2(2, 2))
    val tall   = List(Vector2(0, 0), Vector2(0, 1), Vector2(0, 2), Vector2(0, 3))
    val square = List(Vector2(0, 0), Vector2(1, 0), Vector2(0, 1), Vector2(1, 1))

    val all = Seq(flat, plus, el, tall, square)

    def rockIterator = repeatedly(all.zipWithIndex)
  end Rock

  type Input = String
  def parse(xs: Iterator[String]): Input = xs.next

  def repeatedly[T](xs: Iterable[T]) = new Iterator[T] {
    var it                        = xs.iterator
    override def hasNext: Boolean = true
    override def next(): T =
      if !it.hasNext then it = xs.iterator
      it.next()
  }

  def jetIterator(jets: String) = repeatedly(jets.map {
    case '>' => Jet.Right
    case '<' => Jet.Left
  }.zipWithIndex)

  case class Cavern(rocks: Set[Vector2], height: Int, count: Int, rockIndex: Int, jetIndex: Int):
    val width = 7
    def isValid(rock: Rock, position: Vector2): Boolean =
      rock
        .map(p => p.plus(position))
        .forall(p => p.x >= 0 && p.x < width && p.y >= 0 && !rocks.contains(p))

  def rockFall(cavern: Cavern, rocksIterator: Iterator[(Rock, Int)], jetsIterator: Iterator[(Jet, Int)]): Cavern =
    val (rock, rockIndex) = rocksIterator.next
    var jetIndex          = -1

    def push(p: Vector2): Vector2 =
      val (j, i) = jetsIterator.next
      jetIndex = i
      val q = p.plus(j.vector)
      if cavern.isValid(rock, q) then q else p

    def fall(p: Vector2): (Vector2, Boolean) =
      val q = p.plus(down)
      if cavern.isValid(rock, q) then (q, true) else (p, false)

    def recurse(p0: Vector2): Vector2 =
      val (p1, falling) = fall(push(p0))
      if falling then recurse(p1) else p1

    val startPosition = Vector2(2, cavern.height + 3)
    val finalPosition = recurse(startPosition)
    val finalRock     = rock.map(finalPosition.plus(_))
    Cavern(
      rocks = finalRock.foldLeft(cavern.rocks) { (rs, p) => rs + p },
      height = math.max(cavern.height, finalRock.maxBy(_.y).y + 1),
      count = cavern.count + 1,
      rockIndex,
      jetIndex
    )
  end rockFall

  def part1(input: Input) =
    val jets    = jetIterator(input)
    val rocks   = Rock.rockIterator
    val initial = Cavern(Set.empty, 0, 0, -1, -1)
    val states  = Iterator.iterate(initial) { c => rockFall(c, rocks, jets) }
    states.drop(2022).next.height

  def ceiling(cavern: Cavern): Vector[Int] =
    var ceiling = Vector.fill(7)(Int.MinValue)
    cavern.rocks.foreach { p =>
      if ceiling(p.x) < p.y then ceiling = ceiling.updated(p.x, p.y)
    }
    ceiling.map(_ - cavern.height)

  case class State(ceiling: Vector[Int], blockIndex: Int, jetIndex: Int)

  def part2(input: Input) =
    val targetBlocks = 1_000_000_000_000L
    val jets         = jetIterator(input)
    val rocks        = repeatedly(Rock.all.zipWithIndex)
    val initial      = Cavern(Set.empty, 0, 0, -1, -1)

    @tailrec def recurse(cavern: Cavern, seen: Map[State, (Int, Int)]): Long =
      val nextCavern = rockFall(cavern, rocks, jets)
      val state      = State(ceiling(nextCavern), nextCavern.rockIndex, nextCavern.jetIndex)
      if !seen.contains(state) then recurse(nextCavern, seen + (state -> (nextCavern.count, nextCavern.height)))
      else
        val (blocks, height) = seen(state)
        val loopBlocks       = (nextCavern.count - blocks).toLong
        val loopHeight       = (nextCavern.height - height).toLong
        val nLoops           = (targetBlocks - blocks) / loopBlocks
        val heightWithLoops  = height + nLoops * loopHeight
        val remBlocks        = targetBlocks - blocks - (nLoops * loopBlocks)
        val finalCavern = (0L until remBlocks).foldLeft(nextCavern) { (prev, _) =>
          rockFall(prev, rocks, jets)
        }
        (finalCavern.height - cavern.height) + heightWithLoops
    recurse(initial, Map.empty)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
