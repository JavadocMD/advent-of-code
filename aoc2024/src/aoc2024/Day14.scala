package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day14 extends Day:

  case class Vector2(x: Long, y: Long):
    def +(that: Vector2): Vector2 = Vector2(this.x + that.x, this.y + that.y)
    def *(that: Vector2): Vector2 = Vector2(this.x * that.x, this.y * that.y)

  object Vector2:
    def s(x: String, y: String): Vector2 = Vector2(x.toLong, y.toLong)

  val sizeX = 101
  val sizeY = 103
  val halfX = sizeX / 2
  val halfY = sizeY / 2

  case class Robit(pos: Vector2, vel: Vector2):
    def update(times: Int): Robit =
      val p = pos + (vel * Vector2(times, times))
      val x =
        if p.x < 0 then
          val dx = p.x.abs % sizeX
          if dx > 0 then sizeX - dx else 0
        else p.x % sizeX
      val y =
        if p.y < 0 then
          val dy = p.y.abs % sizeY
          if dy > 0 then sizeY - dy else 0
        else p.y % sizeY
      this.copy(pos = Vector2(x, y))

  type Input = List[Robit]

  def parse(xs: List[String]): Input =
    xs.map:
      case s"p=$px,$py v=$vx,$vy" => Robit(Vector2.s(px, py), Vector2.s(vx, vy))

  def quadrantCount(robits: List[Robit]): (Long, Long, Long, Long) =
    robits.iterator
      .map: r =>
        r.pos match
          case Vector2(x, y) if x < halfX && y < halfY => (1, 0, 0, 0)
          case Vector2(x, y) if x < halfX && y > halfY => (0, 1, 0, 0)
          case Vector2(x, y) if x > halfX && y < halfY => (0, 0, 1, 0)
          case Vector2(x, y) if x > halfX && y > halfY => (0, 0, 0, 1)
          case _                                       => (0, 0, 0, 0)
      .foldLeft((0L, 0L, 0L, 0L)):
        case ((a, b, c, d), (e, f, g, h)) => (a + e, b + f, c + g, d + h)

  def part1(input: Input) =
    val robits       = input.map(_.update(100))
    val (a, b, c, d) = quadrantCount(robits)
    a * b * c * d

  def draw(robits: List[Robit]): Unit =
    val occupied = robits.view.map(_.pos).toSet
    val rows = for y <- 0 until sizeY yield
      val line = for x <- 0 until sizeX yield if occupied.contains(Vector2(x, y)) then "#" else "."
      line.mkString
    println(rows.mkString("\n"))

  val treeShape = Seq(
    // three below
    Vector2(0, 1),
    Vector2(-1, 1),
    Vector2(1, 1),
    // five below that
    Vector2(0, 2),
    Vector2(-1, 2),
    Vector2(1, 2),
    Vector2(-2, 2),
    Vector2(2, 2),
    // seven below that
    Vector2(0, 3),
    Vector2(-1, 3),
    Vector2(1, 3),
    Vector2(-2, 3),
    Vector2(2, 3),
    Vector2(-3, 3),
    Vector2(3, 3),
  )

  def containsTree(robits: List[Robit]): Boolean =
    val occupied = robits.view.map(_.pos).toSet
    robits
      .find: r =>
        treeShape.forall(v => occupied.contains(v + r.pos))
      .isDefined

  def part2(input: Input) =
    val hasTree = Iterator
      .iterate(input)(_.map(_.update(1)))
      .take(10000)
      .zipWithIndex
      .find((robits, i) => containsTree(robits))

    hasTree match
      case Some((rs, i)) => /*draw(rs);*/ i
      case None          => throw Exception("couldn't find it")

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
