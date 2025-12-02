package aoc2021

import scala.annotation.tailrec
import scala.collection.View

import aoc.Day

object Day19 extends Day:
  // NOTE: my initial attempt used this AxisAngle representation
  // for rotations. For some reason it doesn't work for all of the
  // angles. Instead I borrowed the Transform representation from
  // @FlorianCassayre's solution in order to proceed. Still,
  // this solution runs extremely slowly and could no doubt be improved.

  // MATH & UTILITIES

  sealed trait Axis(val vector: Vector)
  object Axis:
    case object Xp extends Axis(Vector(1, 0, 0))
    case object Xn extends Axis(Vector(-1, 0, 0))
    case object Yp extends Axis(Vector(0, 1, 0))
    case object Yn extends Axis(Vector(0, -1, 0))
    case object Zp extends Axis(Vector(0, 0, 1))
    case object Zn extends Axis(Vector(0, 0, -1))
    val all = Seq(Xp, Xn, Yp, Yn, Zp, Zn)

  sealed trait Angle(val sin: Int, val cos: Int)
  object Angle:
    case object D000 extends Angle(0, 1)
    case object D090 extends Angle(1, 0)
    case object D180 extends Angle(0, -1)
    case object D270 extends Angle(-1, 0)
    val all = Seq(D000, D090, D180, D270)

  case class AxisAngle(x: Axis, a: Angle):
    def rotate(v: Vector): Vector =
      val e = x.vector
      (v * a.cos) +
        (e.cross(v) * a.sin) +
        (e * (1 - a.cos) * (e.dot(v)))

  object AxisAngle:
    val unit = AxisAngle(Axis.Xp, Angle.D000)
    val all  = for x <- Axis.all; a <- Angle.all yield AxisAngle(x, a)

  case class Transform(xs: Seq[(Int, Int)]):
    def rotate(v: Vector): Vector =
      val coordinates = IndexedSeq(v.x, v.y, v.z)
      xs.map { case (i, sign) => sign * coordinates(i) } match {
        case Seq(x, y, z) => Vector(x, y, z)
      }

  object Transform:
    val unit = Transform(Seq((0, 1), (1, 1), (2, 1)))
    val all = Seq(1, 1, 1, -1, -1, -1)
      .combinations(3)
      .flatMap(_.permutations)
      .flatMap(signs =>
        (0 until 3).permutations
          .map(permutation => Transform(permutation.zip(signs)))
      )
      .toSeq

  type Rotation = Transform
  val Rotation = Transform

  case class Vector(x: Int, y: Int, z: Int):
    def +(that: Vector): Vector = Vector(x + that.x, y + that.y, z + that.z)
    def -(that: Vector): Vector = Vector(x - that.x, y - that.y, z - that.z)
    def *(scalar: Int): Vector  = Vector(x * scalar, y * scalar, z * scalar)
    def negate: Vector          = Vector(-x, -y, -z)
    lazy val mag2: Int          = x * x + y * y + z * z
    lazy val manhattan: Int     = Math.abs(x) + Math.abs(y) + Math.abs(z)

    def cross(that: Vector): Vector = Vector(
      (this.y * that.z - this.z * that.y),
      (this.z * that.x - this.x * that.z),
      (this.x * that.y - this.y * that.x)
    )
    def dot(that: Vector): Int =
      this.x * that.x + this.y * that.y + this.z * that.z
  end Vector

  // BEGIN SOLUTION

  type Id = Int
  case class Scanner(id: Id, beacons: Seq[Vector]):
    override def toString = s"Scanner($id)"
    def adjust(rotation: Rotation, offset: Vector) =
      copy(beacons = beacons.map(v => rotation.rotate(v) + offset))

  // The relationship between two scanners. To make the *second* scanner's beacons
  // align with the first, given such a relationship, first apply rotation
  // then offset to a scanner's beacons.
  type Relation = (Rotation, Vector)

  type Input = Seq[Scanner]

  def parse(xs: Array[String]): Input =
    splitChunks(xs).map(chunk =>
      val id = chunk.head match
        case s"--- scanner $id ---" => id.toInt
      val bs = chunk.tail.map { case s"$x,$y,$z" =>
        Vector(x.toInt, y.toInt, z.toInt)
      }
      Scanner(id, bs)
    )

  def findRelation(s1: Scanner, s2: Scanner): Option[Relation] =
    val as = s1.beacons.toSet
    val pairs = for
      rotation <- Rotation.all.view
      bs = s2.beacons.map(rotation.rotate).toSet
      p1 <- as
      p2 <- bs
      offset = p1 - p2
    yield (rotation, offset, bs.map(_ + offset))
    pairs
      .find((_, offset, bs) => (as intersect bs).size >= 12)
      .map((rotation, offset, _) => (rotation, offset))

  type Links = Map[Int, Seq[Relation]]
  def findAllRelations(scanners: Seq[Scanner]): Links =
    var linked   = scanners.take(1).toSet
    var unlinked = scanners.drop(1).toSet

    var open  = scanners.take(1)
    var links = Map[Int, List[Relation]](0 -> Nil)

    while open.nonEmpty do
      val s1   = open.head
      val rels = unlinked.flatMap(s2 => findRelation(s1, s2).map(rel => (s2, rel)))
      if (rels.isEmpty)
        open = open.tail // no more relations for this starting point
      else
        rels.foreach((s2, rel) =>
          println(s"found ${s1.id} -> ${s2.id}")
          linked += s2
          unlinked -= s2
          open :+= s2
          val chain = rel :: links(s1.id)
          links += (s2.id -> chain)
        )

    assert(links.size == scanners.size)
    links

  def normalizePoints(scanners: Seq[Scanner], links: Links): Set[Vector] =
    val beacons =
      for s <- scanners
      yield
        val rels = links(s.id)
        s.beacons.map(v => rels.foldLeft(v)((v, r) => r._1.rotate(v) + r._2))
    beacons.foldLeft(Set.empty)((acc, bs) => acc ++ bs)

  def part1(input: Input, links: Links) =
    val links = findAllRelations(input)
    val ps    = normalizePoints(input, links)
    ps.size

  def part2(input: Input, links: Links) =
    val positions =
      for s <- input
      yield
        val rels = links(s.id)
        rels.foldLeft(Vector(0, 0, 0))((v, r) => r._1.rotate(v) + r._2)
    val dists = for
      p1 <- positions
      p2 <- positions
      if p1 != p2
    yield (p2 - p1).manhattan
    dists.max

  final def main(args: Array[String]): Unit =
    val in         = parse(loadInput())
    lazy val links = findAllRelations(in)
    solveP1(() => part1(in, links))
    solveP2(() => part2(in, links))
