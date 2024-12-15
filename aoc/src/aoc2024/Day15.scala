package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day15 extends Day:

  case class Vector2(x: Long, y: Long):
    def +(that: Vector2): Vector2 = Vector2(this.x + that.x, this.y + that.y)
    def -(that: Vector2): Vector2 = Vector2(this.x - that.x, this.y - that.y)
    def *(that: Vector2): Vector2 = Vector2(this.x * that.x, this.y * that.y)
    def unit: Vector2 = Vector2(
      if x == 0 then 0 else if x < 0 then -1 else 1,
      if y == 0 then 0 else if y < 0 then -1 else 1,
    )

  object Vector2:
    def s(x: String, y: String): Vector2 = Vector2(x.toLong, y.toLong)
    def range(a: Vector2, b: Vector2, inclusive: Boolean = false): Iterator[Vector2] =
      val dir = (b - a).unit
      val end = if inclusive then b + dir else b
      if !Direction.all.contains(dir) then throw Exception("not a cardinal direction!")
      Iterator.iterate(a)(_ + dir).takeWhile(_ != end)

  object Direction:
    val E   = Vector2(1, 0)
    val S   = Vector2(0, 1)
    val W   = Vector2(-1, 0)
    val N   = Vector2(0, -1)
    val all = Seq(E, S, W, N)

  type Grid  = Map[Vector2, Char]
  type Input = (Grid, List[Vector2])

  def parse(xs: List[String]): Input =
    val (gridLines, moveLines) = xs.splitAt(xs.indexWhere(_.isBlank))
    val grid = Map.from(
      for
        (line, y) <- gridLines.zipWithIndex
        (tile, x) <- line.zipWithIndex
      yield Vector2(x, y) -> tile
    )
    val moves = moveLines
      .reduce(_ + _)
      .map:
        case '^' => Direction.N
        case '>' => Direction.E
        case 'v' => Direction.S
        case '<' => Direction.W
      .toList
    (grid, moves)

  def pushScan(grid: Grid, pos: Vector2, dir: Vector2): Option[Vector2] =
    val nonBox = Iterator
      .iterate(pos + dir)(_ + dir)
      .takeWhile(grid.contains(_))
      .find(grid(_) != 'O')
    // can push as long as we didn't hit a wall
    nonBox.flatMap(p => if grid(p) == '.' then Some(p) else None)

  case class State(grid: Grid, robot: Vector2)

  object State:
    def initial(grid: Grid): State =
      val start = grid.find((_, t) => t == '@').get._1
      State(grid.updated(start, '.'), start)

  def makeMove(state: State, move: Vector2): State =
    val nextRobot = state.robot + move
    val nextOpen  = pushScan(state.grid, state.robot, move)
    nextOpen match
      case Some(p) if p == nextRobot =>
        // robot moving without pushing
        State(state.grid, nextRobot)
      case Some(p) =>
        // robot moves and pushes a box to `p`
        val nextGrid = state.grid.updated(nextRobot, '.').updated(p, 'O')
        State(nextGrid, nextRobot)
      case None =>
        // robot can't move
        state

  def calcGps(grid: Grid, boxChar: Char = 'O'): Long =
    grid
      .filter((_, t) => t == boxChar)
      .map((p, _) => 100 * p.y + p.x)
      .sum

  def part1(input: Input) =
    val (grid, moves) = input
    val finalState    = moves.foldLeft(State.initial(grid))(makeMove(_, _))
    calcGps(finalState.grid)

  def expandGrid(grid: Grid): Grid =
    grid.flatMap: (p, t) =>
      val a = p * Vector2(2, 1)
      val b = a + Direction.E
      t match
        case '#' => (a -> '#') :: (b -> '#') :: Nil
        case 'O' => (a -> '[') :: (b -> ']') :: Nil
        case '.' => (a -> '.') :: (b -> '.') :: Nil
        case '@' => (a -> '@') :: (b -> '.') :: Nil

  def pushScanEW(grid: Grid, start: Vector2, dir: Vector2): Option[List[Vector2]] =
    @tailrec
    def recurse(curr: Vector2, acc: List[Vector2]): Option[List[Vector2]] =
      grid(curr) match
        case '#' => None
        case '.' => Some(acc)
        case '[' => recurse(curr + dir, curr :: acc)
        case ']' => recurse(curr + dir, acc)
    recurse(start, Nil)

  def pushScanNS(grid: Grid, start: Vector2, dir: Vector2): Option[List[Vector2]] =
    @tailrec
    def recurse(open: Set[Vector2], acc: Set[Vector2]): Option[Set[Vector2]] =
      if open.isEmpty then Some(acc)
      else
        val curr = open.head
        grid(curr) match
          case '#' => None // if anyone hits a wall, game over
          case '.' => recurse(open.tail, acc)
          case '[' =>
            val next = (curr + dir) :: (curr + dir + Direction.E) :: Nil
            recurse(open.tail ++ next, acc + curr)
          case ']' =>
            val next = (curr + dir) :: (curr + dir + Direction.W) :: Nil
            recurse(open.tail ++ next, acc + (curr + Direction.W))
    recurse(Set(start), Set.empty).map(_.toList)

  def moveBoxes(grid: Grid, boxes: List[Vector2], dir: Vector2): Grid =
    val blanked = boxes.foldLeft(grid): (g, b) =>
      g.updated(b, '.').updated(b + Direction.E, '.')
    boxes.foldLeft(blanked): (g, b) =>
      g.updated(b + dir, '[').updated(b + dir + Direction.E, ']')

  def makeMove2(state: State, move: Vector2): State =
    val nextRobot = state.robot + move
    val scanFn = move match
      case Direction.E | Direction.W => pushScanEW
      case Direction.N | Direction.S => pushScanNS
    scanFn(state.grid, nextRobot, move) match
      case None => state // robot can't move
      case Some(boxes) =>
        val nextGrid = moveBoxes(state.grid, boxes, move)
        State(nextGrid, nextRobot)

  def draw(grid: Grid, size: Vector2): Unit =
    val lines =
      for y <- 0L until size.y
      yield (for x <- 0L until size.x yield grid(Vector2(x, y))).mkString
    println(lines.mkString("\n"))

  def part2(input: Input) =
    val (smallGrid, moves) = input
    val grid               = expandGrid(smallGrid)
    val finalState         = moves.foldLeft(State.initial(grid))(makeMove2(_, _))
    calcGps(finalState.grid, '[')

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
