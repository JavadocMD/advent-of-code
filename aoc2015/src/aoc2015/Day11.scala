package aoc2015

import scala.annotation.tailrec
import aoc.Day

object Day11 extends Day:

  type Input = String

  def parse(xs: List[String]): Input = xs.head

  lazy val input: Input = parse(loadInput().toList)

  def increment(chars: List[Char]): List[Char] = chars match
    case Nil         => Nil
    case 'z' :: tail => 'a' :: increment(tail)
    case n :: tail   => (n + 1).toChar :: tail

  def isValid(chars: List[Char]): Boolean =
    @tailrec
    def hasInvalidChars(chars: List[Char]): Boolean = chars match
      case 'i' :: tail => true
      case 'o' :: tail => true
      case 'l' :: tail => true
      case _ :: tail   => hasInvalidChars(tail)
      case Nil         => false

    def hasStraight(chars: List[Char]): Boolean =
      chars
        .sliding(3)
        .find:
          case a :: b :: c :: Nil => a - 1 == b && b - 1 == c
          case _                  => false
        .isDefined

    def hasTwoPairs(chars: List[Char]): Boolean =
      val pairIndices = chars.zipWithIndex
        .sliding(2)
        .collect:
          case ((a, i) :: (b, _) :: Nil) if a == b => i
        .toList

      if pairIndices.size < 2 then false
      else
        pairIndices
          .combinations(2)
          .find:
            case (i :: j :: Nil) => (j - i).abs > 1
            case _               => false
          .isDefined

    !hasInvalidChars(chars) && hasStraight(chars) && hasTwoPairs(chars)
  end isValid

  def nextValid(password: String): String =
    val result = Iterator
      .iterate(password.toList.reverse)(increment)
      .drop(1)
      .find(isValid)
    result.get.reverse.mkString

  lazy val part1 = nextValid(input)

  lazy val part2 = nextValid(part1)

  final def main(args: Array[String]): Unit =
    solveP1(() => part1)
    solveP2(() => part2)
