package aoc2019

import aoc._
import scala.annotation.tailrec

import aoc.Day

object Day17 extends Day:
  import TwoDee._
  import Directionality.OriginTopLeft
  import IntcodeComputer as C

  type Input = C.Program
  def parse(xs: Array[String]): Input = C.parseMemory(xs.head)

  object Cell:
    val Unknown  = 63 // ?
    val Scaffold = 35 // #
    val Start    = 94 // ^

    def draw(x: Int): String = x.toChar.toString

  def readMap(output: Seq[Long]): Grid[Int] =
    var grid = Grid.create[Int](Cell.Unknown)
    var x    = 0
    var y    = 0
    output.foreach({
      case 10 =>
        x = 0
        y += 1
      case c =>
        grid = grid.updated(Vector(x, y), c.toInt)
        x += 1
    })
    grid

  def findIntersections(grid: Grid[Int]): Seq[Vector] =
    def isIntersection(vector: Vector): Boolean =
      grid(vector) == Cell.Scaffold && vector.neighbors.forall(x => grid(x) == Cell.Scaffold)

    grid.keys.filter(isIntersection).toSeq

  final def digits(n: Int): List[Int] =
    @tailrec
    def f(n: Int, acc: List[Int]): List[Int] = {
      if (n < 10) n :: acc
      else f(n / 10, n % 10 :: acc)
    }
    f(n, Nil)

  final def flattenAndSeparate[T](xs: List[List[T]], separator: T, trailing: Boolean): List[T] =
    @tailrec
    def f(curr: List[List[T]], acc: List[T]): List[T] = curr match
      case Nil          => Nil
      case last :: Nil  => last ::: acc
      case head :: tail => f(tail, separator :: (head ::: acc))

    val accInit = if (trailing) separator :: Nil else Nil
    f(xs.reverse, accInit)

  object AsciiValue:
    val Comma = 44L
    val Line  = 10L
    val Yes   = 121L
    val No    = 110L

  sealed trait AsciiInput:
    val code: List[Long]

  object AsciiInput:
    def toCodeSequence(xs: List[AsciiInput]): List[Long] =
      flattenAndSeparate(xs.map(_.code), AsciiValue.Comma, false)

  sealed trait Move extends AsciiInput
  object Move:
    case object Left extends Move:
      val code              = List(76L)
      override def toString = "L"

    case object Right extends Move:
      val code              = List(82L)
      override def toString = "R"

    case class Forward(n: Int) extends Move:
      val code              = digits(n).map(_ + 48L)
      override def toString = n.toString
  end Move

  sealed trait FunctionCall extends AsciiInput
  object FunctionCall:
    case object A extends FunctionCall { val code = List(65L) }
    case object B extends FunctionCall { val code = List(66L) }
    case object C extends FunctionCall { val code = List(67L) }

  def walk(grid: Grid[Int], start: Vector): List[Move] =
    // first move is a left turn
    var moves: List[Move] = Move.Left :: Nil
    var curr              = start
    var facing: Direction = Direction.West

    def forward(): Move.Forward =
      val dir  = facing.toVector
      var step = 0
      var next = curr + dir
      while (grid(next) == Cell.Scaffold) {
        step += 1
        next += dir
      }
      curr = next - dir
      Move.Forward(step)

    def turn(): Option[Move] =
      val left  = curr + Direction.turnLeft(facing).toVector
      val right = curr + Direction.turnRight(facing).toVector
      if (grid(left) == Cell.Scaffold) {
        facing = Direction.turnLeft(facing)
        Some(Move.Left)
      } else if (grid(right) == Cell.Scaffold) {
        facing = Direction.turnRight(facing)
        Some(Move.Right)
      } else None

    var prev: Option[Move] = Some(Move.Left)
    while (prev.isDefined) {
      moves ::= forward()
      prev = turn()
      prev match {
        case Some(m) => moves ::= m
        case None    => // no-op
      }
    }

    moves.reverse
  end walk

  // it became clear during debugging that grouping the commands by pairs -- a turn followed by a forward --
  // would produce good results. this codec encodes unique pairs as a symbol which makes
  // searching for a good compression easier

  type Encoding = ((Move, Move), Char)

  object Codec:
    def forMoves(moves: Seq[Move]): Codec =
      lazy val symbols: Stream[Char] = 'a' #:: symbols.map(c => (c + 1).toChar)
      val pairs                      = moves.toList.grouped(2).distinct.zip(symbols).toSeq
      val dict = pairs.map[Encoding]({
        case (m1 :: m2 :: Nil, char) => ((m1, m2), char)
        case _                       => throw new Exception("Odd number of moves!")
      })
      Codec(dict)

  case class Codec(dict: Seq[Encoding]):
    def encode(moves: Seq[Move], acc: String = ""): String =
      if (moves.length % 2 != 0) throw new Exception("Odd number of moves!")
      moves
        .grouped(2)
        .foldLeft("")({ case (acc, m1 :: m2 :: Nil) =>
          dict.find({ case (pair, c) => pair == (m1, m2) }) match
            case None            => throw new Exception("Pair not in dictionary!")
            case Some((_, char)) => acc + char
        })

    def decode(encoded: String): List[Move] = encoded.reverseIterator
      .foldLeft(List.empty[Move])({ case (acc, char) =>
        dict.find({ case (e, c) => c == char }) match
          case None                => throw new Exception("Char not in dictionary!")
          case Some(((m1, m2), _)) => m1 :: m2 :: acc
      })

  /** * Find the first contiguous sequence of given length that is not in the excluded set.
    */
  @tailrec
  final def uniqueSequence(corpus: String, length: Int, exclude: Seq[String], i: Int = 0): String =
    exclude.find(x => corpus.indexOf(x, i) == i) match
      case None    => corpus.substring(i, i + length)
      case Some(e) => uniqueSequence(corpus, length, exclude, i + e.size)

  /** * Check if the given set of substrings cover the corpus exactly.
    */
  @tailrec
  final def coveringSet(corpus: String, substrings: Seq[String], i: Int = 0): Boolean =
    if (i == corpus.length) true
    else
      substrings.find(x => corpus.indexOf(x, i) == i) match
        case None    => false
        case Some(s) => coveringSet(corpus, substrings, i + s.size)

  // the program input is a compression of the move sequence using three subsequences (functions A/B/C)
  // and the "continuous output" config which I didn't try to use

  case class MovementLogic(
      main: List[FunctionCall],
      funcA: List[Move],
      funcB: List[Move],
      funcC: List[Move],
      continuous: Boolean
  ):
    def toInput: List[Long] = flattenAndSeparate(
      List(
        AsciiInput.toCodeSequence(main),
        AsciiInput.toCodeSequence(funcA),
        AsciiInput.toCodeSequence(funcB),
        AsciiInput.toCodeSequence(funcC),
        List(if (continuous) AsciiValue.Yes else AsciiValue.No)
      ),
      AsciiValue.Line,
      true
    )

  def compressLogic(moves: Seq[Move]): MovementLogic =
    val codec = Codec.forMoves(moves)
    val ms    = codec.encode(moves)

    // brute force search for command sequences for functions A, B, and C
    // after 6 command sequences, the function would be too long (more than 20 chars)
    val solutions = for {
      ai <- 1 to 6
      bi <- 1 to 6
      ci <- 1 to 6
      a = uniqueSequence(ms, ai, Seq())
      b = uniqueSequence(ms, bi, Seq(a))
      c = uniqueSequence(ms, ci, Seq(a, b))
      if coveringSet(ms, Seq(a, b, c))
    } yield (a, b, c)

    if (solutions.length == 0) throw new Exception("No solution found.")
    val (a, b, c) = solutions(0)

    val main = ms
      .replace(a, "A")
      .replace(b, "B")
      .replace(c, "C")
      .iterator
      .map[FunctionCall]({
        case 'A' => FunctionCall.A
        case 'B' => FunctionCall.B
        case 'C' => FunctionCall.C
        case _   => throw new Exception("Unexpected function call.")
      })
      .toList

    MovementLogic(
      main,
      codec.decode(a),
      codec.decode(b),
      codec.decode(c),
      false // continuous feed?
    )
  end compressLogic

  def partTwo(program: C.Program, grid: Grid[Int]): Long =
    val start = grid.find({ case (v, c) => c == Cell.Start }).map(_._1).get
    val moves = walk(grid, start)
    val input = compressLogic(moves).toInput
    val s0    = C.State(program.updated(0, 2), input.iterator)
    val sf    = C.run(s0)
    sf.output.last

  def part1(input: Input): Long =
    val s0   = C.State(input, Iterator.empty)
    val sf   = C.run(s0)
    val grid = readMap(sf.output)
    findIntersections(grid).map(v => v.x * v.y).sum

  def part2(input: Input): Long =
    val s0   = C.State(input, Iterator.empty)
    val sf   = C.run(s0)
    val grid = readMap(sf.output)
    partTwo(input, grid)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
