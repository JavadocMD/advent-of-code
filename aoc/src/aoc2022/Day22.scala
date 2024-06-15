package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day22 extends Day:
  case class Vector2(x: Int, y: Int):
    def step(f: Facing): Vector2 = f match
      case Facing.Right => Vector2(x + 1, y)
      case Facing.Down  => Vector2(x, y + 1)
      case Facing.Left  => Vector2(x - 1, y)
      case Facing.Up    => Vector2(x, y - 1)
  end Vector2

  enum Tile:
    case Empty
    case Open
    case Wall
  object Tile:
    def parse(c: Char): Tile = c match
      case ' ' => Empty
      case '.' => Open
      case '#' => Wall
  import Tile._

  enum Move:
    case Forward(steps: Int)
    case Left
    case Right

  enum Facing:
    case Right, Down, Left, Up
    def turnLeft: Facing = this match
      case Right => Up
      case Down  => Right
      case Left  => Down
      case Up    => Left
    def turnRight: Facing = this match
      case Right => Down
      case Down  => Left
      case Left  => Up
      case Up    => Right
    def opposite: Facing = this match
      case Right => Left
      case Down  => Up
      case Left  => Right
      case Up    => Down
  import Facing._

  enum Face(val corner: Vector2):
    case A extends Face(Vector2(50, 0))
    case B extends Face(Vector2(100, 0))
    case C extends Face(Vector2(50, 50))
    case D extends Face(Vector2(0, 100))
    case E extends Face(Vector2(50, 100))
    case F extends Face(Vector2(0, 150))
  object Face:
    val width = 50
    val all   = Seq(A, B, C, D, E, F)
    def current(p: Vector2): Face = all.find { f =>
      val Vector2(cx, cy) = f.corner
      cx <= p.x && p.x < cx + width &&
      cy <= p.y && p.y < cy + width
    }.get
  end Face

  object Traversal:
    // distance is [0,49]
    def invert(distance: Int): Int = Face.width - 1 - distance

    def edgeDistance(face: Face, edge: Facing, position: Vector2): Int =
      val Vector2(cx, cy) = face.corner
      edge match
        case Up    => position.x - cx
        case Right => position.y - cy
        case Down  => invert(position.x - cx)
        case Left  => invert(position.y - cy)

    def alongEdge(face: Face, edge: Facing, distance: Int): Vector2 =
      val Vector2(cx, cy) = face.corner
      edge match
        case Up    => Vector2(cx + distance, cy)
        case Right => Vector2(cx + Face.width - 1, cy + distance)
        case Down  => Vector2(cx + invert(distance), cy + Face.width - 1)
        case Left  => Vector2(cx, cy + invert(distance))

    def traverseFlat(face: Face, edge: Facing, toFace: Face, toEdge: Facing)(p: Vector2): (Vector2, Facing) =
      val d = invert(edgeDistance(face, edge, p))
      val v = alongEdge(toFace, toEdge, d)
      (v, edge)

    def traverseCube(face: Face, edge: Facing, toFace: Face, toEdge: Facing)(p: Vector2): (Vector2, Facing) =
      val d = invert(edgeDistance(face, edge, p))
      val v = alongEdge(toFace, toEdge, d)
      (v, toEdge.opposite)
  end Traversal

  type FaceEdge    = (Face, Facing)
  type Traverse    = Vector2 => (Vector2, Facing)
  type Connections = Map[FaceEdge, Traverse]

  val flatConnections: Map[FaceEdge, Traverse] = Seq(
    (Face.A, Up, Face.E, Down),    // A0 <-> E3
    (Face.A, Left, Face.B, Right), // A3 <-> B1
    (Face.B, Up, Face.B, Down),    // B0 <-> B3
    (Face.C, Right, Face.C, Left), // C1 <-> C3
    (Face.D, Left, Face.E, Right), // D3 <-> E1
    (Face.D, Up, Face.F, Down),    // D0 <-> F2
    (Face.F, Left, Face.F, Right)  // F3 <-> F1
  ).flatMap { (f0, e0, f1, e1) =>
    Seq(
      (f0, e0) -> Traversal.traverseFlat(f0, e0, f1, e1),
      (f1, e1) -> Traversal.traverseFlat(f1, e1, f0, e0)
    )
  }.toMap

  val cubeConnections: Map[FaceEdge, Traverse] = Seq(
    (Face.A, Up, Face.F, Left),     // A0 <-> F3
    (Face.A, Left, Face.D, Left),   // A3 <-> D3
    (Face.B, Up, Face.F, Down),     // B0 <-> F2
    (Face.B, Right, Face.E, Right), // B1 <-> E1
    (Face.B, Down, Face.C, Right),  // B2 <-> C1
    (Face.C, Left, Face.D, Up),     // C3 <-> D0
    (Face.E, Down, Face.F, Right)   // E2 <-> F1
  ).flatMap { (f0, e0, f1, e1) =>
    Seq(
      (f0, e0) -> Traversal.traverseCube(f0, e0, f1, e1),
      (f1, e1) -> Traversal.traverseCube(f1, e1, f0, e0)
    )
  }.toMap

  type Grid = Map[Vector2, Tile]

  case class Input(grid: Grid, moves: List[Move])

  def parse(xs: Iterator[String]): Input =
    val (xs0, xs1)  = xs.span(_ != "")
    val movesString = xs1.drop(1).next

    val grid = (for
      (line, j) <- xs0.zipWithIndex
      (char, i) <- line.zipWithIndex
    yield Vector2(i, j) -> Tile.parse(char)).toMap.withDefault(_ => Empty)

    import Move._
    def parseMoves(xs: List[Char], num: Int = 0, acc: List[Move] = Nil): List[Move] = xs match
      case Nil          => (Forward(num) :: acc).reverse
      case 'L' :: tail  => parseMoves(tail, 0, Left :: Forward(num) :: acc)
      case 'R' :: tail  => parseMoves(tail, 0, Right :: Forward(num) :: acc)
      case head :: tail => parseMoves(tail, num * 10 + (head - '0'), acc)

    Input(grid, parseMoves(movesString.toList))
  end parse

  def start(input: Input): Vector2 = Iterator
    .from(0)
    .map(Vector2(_, 0))
    .find { v => input.grid(v) == Open }
    .get

  def run(input: Input, connections: Connections): (Vector2, Facing) =
    // recursively go forward until we hit a wall or run out of steps
    def goForward(steps: Int, curr: Vector2, dir: Facing): (Vector2, Facing) =
      if steps == 0 then (curr, dir)
      else
        val next = curr.step(dir)
        input.grid(next) match
          case Wall => (curr, dir)
          case Open => goForward(steps - 1, next, dir)
          case Empty =>
            val face     = Face.current(curr)
            val traverse = connections((face, dir))
            val (np, nf) = traverse(curr)
            if input.grid(np) == Wall then (curr, dir)
            else goForward(steps - 1, np, nf)

    // recursively process moves
    def recurse(moves: List[Move], curr: Vector2, dir: Facing): (Vector2, Facing) = moves match
      case Nil                => (curr, dir)
      case Move.Left :: tail  => recurse(tail, curr, dir.turnLeft)
      case Move.Right :: tail => recurse(tail, curr, dir.turnRight)
      case Move.Forward(steps) :: tail =>
        val (v, f) = goForward(steps, curr, dir)
        recurse(tail, v, f)

    recurse(input.moves, start(input), Right)
  end run

  def score(position: Vector2, facing: Facing): Int =
    1000 * (position.y + 1) + 4 * (position.x + 1) + facing.ordinal

  def part1(input: Input) = score.tupled(run(input, flatConnections))

  def part2(input: Input) = score.tupled(run(input, cubeConnections))

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
