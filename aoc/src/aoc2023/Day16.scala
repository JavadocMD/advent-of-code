package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day16 extends Day:

  type Beam = (Point, Dir)

  sealed trait Tile
  case object MirrorLeft         extends Tile // "\"
  case object MirrorRight        extends Tile // "/"
  case object SplitterVertical   extends Tile // "|"
  case object SplitterHorizontal extends Tile // "-"

  case class Input(grid: Grid, tiles: Map[Point, Tile]):
    def lase(beam: Beam): List[Beam] =
      // Evolve the beam based on the tile it's currently on.
      val (p, d) = beam
      tiles.get(p) match
        case None => (p.go(d), d) :: Nil
        case Some(MirrorLeft) =>
          val nextDir = d match
            case Up    => Left
            case Down  => Right
            case Left  => Up
            case Right => Down
          (p.go(nextDir), nextDir) :: Nil
        case Some(MirrorRight) =>
          val nextDir = d match
            case Up    => Right
            case Down  => Left
            case Left  => Down
            case Right => Up
          (p.go(nextDir), nextDir) :: Nil
        case Some(SplitterVertical) =>
          d match
            case Up | Down    => (p.go(d), d) :: Nil
            case Left | Right => (p.go(Up), Up) :: (p.go(Down), Down) :: Nil
        case Some(SplitterHorizontal) =>
          d match
            case Up | Down    => (p.go(Left), Left) :: (p.go(Right), Right) :: Nil
            case Left | Right => (p.go(d), d) :: Nil

  def parse(xs: List[String]): Input =
    val g = Grid(xs)
    val t = g.pointsIterator
      .flatMap:
        case (p, '.')  => None
        case (p, '\\') => Some(p -> MirrorLeft)
        case (p, '/')  => Some(p -> MirrorRight)
        case (p, '|')  => Some(p -> SplitterVertical)
        case (p, '-')  => Some(p -> SplitterHorizontal)
      .toMap
    Input(g, t)

  def beamcast(start: Beam, input: Input): Set[Beam] =
    @tailrec
    def recurse(beams: List[Beam], acc: Set[Beam]): Set[Beam] =
      beams match
        case Nil => acc
        case curr :: tail =>
          input.lase(curr) ::: tail match
            // If the beam goes off the map, it's done.
            case (p, _) :: xs if !input.grid.contains(p) => recurse(xs, acc + curr)
            // If we've seeen the beam before, it's done.
            case x :: xs if acc.contains(x) => recurse(xs, acc + curr)
            // Otherwise, keep on truckin'.
            case xs => recurse(xs, acc + curr)
    end recurse
    recurse(start :: Nil, Set(start))

  def part1(input: Input) =
    val start  = (Point(0, 0), Right)
    val result = beamcast(start, input)
    result.map(_._1).size

  import scala.collection.parallel.CollectionConverters._

  def part2(input: Input) =
    val (w, h) = input.grid.size
    val starts = Seq.concat(
      // Top edge
      (0 until w).map(x => (Point(x, 0 + 0), Down)),
      // Bottom edge
      (0 until w).map(x => (Point(x, h - 1), Up)),
      // Left edge
      (0 until h).map(y => (Point(0 + 0, y), Right)),
      // Right edge
      (0 until h).map(y => (Point(w - 1, y), Left)),
    )
    starts.par
      .map: x =>
        beamcast(x, input).map(_._1).size
      .max

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
