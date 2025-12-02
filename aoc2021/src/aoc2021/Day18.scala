package aoc2021

import scala.annotation.tailrec

import aoc.Day

object Day18 extends Day:

  enum Tree:
    case Pair(left: Tree, right: Tree)
    case Num(value: Int)
  import Tree._

  enum Side:
    case Left
    case Right
  import Side._

  type Path = List[Side]

  object Tree:
    private def splitPair(s: String): (String, String) =
      @tailrec
      def findSplit(i: Int, depth: Int): Int =
        s(i) match
          case ',' if depth == 1 => i
          case '['               => findSplit(i + 1, depth + 1)
          case ']'               => findSplit(i + 1, depth - 1)
          case _                 => findSplit(i + 1, depth)
      val i = findSplit(0, 0)
      (s.slice(1, i), s.slice(i + 1, s.length - 1))

    def parse(s: String): Tree =
      if s(0) != '[' then Num(s.toInt)
      else
        val (left, right) = splitPair(s)
        Pair(parse(left), parse(right))

    def subtree(x: Tree, path: Path): Tree = (x, path) match
      case (_, Nil)                        => x
      case (Pair(left, _), Left :: tail)   => subtree(left, tail)
      case (Pair(_, right), Right :: tail) => subtree(right, tail)
      case _                               => throw new Exception("invalid path")

    def replace(curr: Tree, path: Path, splice: Tree): Tree = (curr, path) match
      case (_, Nil)                 => splice
      case (p: Pair, Left :: tail)  => Pair(replace(p.left, tail, splice), p.right)
      case (p: Pair, Right :: tail) => Pair(p.left, replace(p.right, tail, splice))
      case _                        => throw new Exception("Invalid path.")

    def magnitude(curr: Tree): Long = curr match
      case Num(n)     => n
      case Pair(a, b) => 3 * magnitude(a) + 2 * magnitude(b)

    def toString(x: Tree): String = x match
      case Pair(left, right) => s"[${toString(left)},${toString(right)}]"
      case Num(value)        => value.toString

    def walkLeft(curr: Tree): Path = curr match
      case Pair(left, _) => Left :: walkLeft(left)
      case Num(_)        => Nil

    def walkRight(curr: Tree): Path = curr match
      case Pair(_, right) => Right :: walkRight(right)
      case Num(_)         => Nil

    def leftNum(root: Tree, path: Path): Option[Path] =
      if path.forall(_ == Left) then None
      else
        val back = path.reverse
          .dropWhile(_ == Left)
          .drop(1)
          .prepended(Left)
          .reverse
        Some(back ::: walkRight(subtree(root, back)))

    def rightNum(root: Tree, path: Path): Option[Path] =
      if path.forall(_ == Right) then None
      else
        val back = path.reverse
          .dropWhile(_ == Right)
          .drop(1)
          .prepended(Right)
          .reverse
        Some(back ::: walkLeft(subtree(root, back)))
  end Tree

  def explode(root: Tree): Option[Tree] =
    // Find the first deeply-nested node.
    def find(curr: Tree, path: Path = Nil): Option[Path] =
      curr match
        case Num(_)                       => None
        case Pair(_, _) if path.size == 4 => Some(path.reverse)
        case Pair(left, right) =>
          find(left, Left :: path) orElse find(right, Right :: path)

    find(root) map { p =>
      val Pair(Num(n1), Num(n2)) = subtree(root, p)
      var result                 = replace(root, p, Num(0))
      leftNum(root, p) match
        case Some(leftPath) =>
          val Num(ln) = subtree(root, leftPath)
          result = replace(result, leftPath, Num(n1 + ln))
        case None => // nada
      rightNum(root, p) match
        case Some(rightPath) =>
          val Num(rn) = subtree(root, rightPath)
          result = replace(result, rightPath, Num(n2 + rn))
        case None => // nada
      result
    }

  def split(root: Tree): Option[Tree] =
    // Find the first num larger than 9.
    def find(curr: Tree, path: Path = Nil): Option[Path] =
      curr match
        case Num(n) if n > 9 => Some(path.reverse)
        case Num(_)          => None
        case Pair(left, right) =>
          find(left, Left :: path) orElse find(right, Right :: path)

    find(root) map { p =>
      val Num(n) = subtree(root, p)
      val splice = Pair(Num(n / 2), Num((n + 1) / 2))
      replace(root, p, splice)
    }

  def reduce(x: Tree): Tree =
    explode(x) orElse split(x) match
      case None    => x
      case Some(y) => reduce(y)

  def add(a: Tree, b: Tree): Tree =
    reduce(Pair(a, b))

  type Input = List[Tree]
  def parse(xs: Array[String]): Input =
    xs.view.map(Tree.parse).toList

  def part1(input: Input) =
    val result = input.tail.foldLeft(input.head) { case (acc, curr) =>
      add(acc, curr)
    }
    magnitude(result)

  def part2(input: Input) =
    val results = for
      x <- input
      y <- input
      if x != y
    yield magnitude(add(x, y))
    results.max

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
