package aoc2023

import scala.annotation.tailrec
import scala.util.Sorting
import aoc.Day

object Day07 extends Day:

  case class Hand(cards: List[Char], bid: Long)

  type CardSet = (Int, Char)

  def discoverSets(cards: List[Char]): List[CardSet] =
    cards.foldLeft(List.empty) { case (sets, card) =>
      sets.partition(_._2 == card) match
        case (Nil, others)             => (1, card) :: others
        case (matching :: Nil, others) => (matching._1 + 1, card) :: others
        case x                         => throw new Exception(s"Unexpected case $x")
    }

  def toHandType(sets: List[CardSet]): Int = sets.map(_._1).sorted.reverse match
    case 5 :: Nil                     => 7
    case 4 :: 1 :: Nil                => 6
    case 3 :: 2 :: Nil                => 5
    case 3 :: 1 :: 1 :: Nil           => 4
    case 2 :: 2 :: 1 :: Nil           => 3
    case 2 :: 1 :: 1 :: 1 :: Nil      => 2
    case 1 :: 1 :: 1 :: 1 :: 1 :: Nil => 1
    case x                            => throw Exception(s"Unexpected card sets: $x")

  def upgradeSets(sets: List[CardSet]): List[CardSet] =
    sets.partition(_._2 == 'J') match
      case (Nil, others)           => sets // no jokers
      case ((5, _) :: Nil, others) => sets // five jokers!
      case ((j, _) :: Nil, others) =>
        val most :: rest = others.sorted.reverse: @unchecked
        (most._1 + j, most._2) :: rest
      case x => throw new Exception(s"Unexpected case $x")

  def cardValueP1(card: Char): Int = card match
    case '2' => 2
    case '3' => 3
    case '4' => 4
    case '5' => 5
    case '6' => 6
    case '7' => 7
    case '8' => 8
    case '9' => 9
    case 'T' => 10
    case 'J' => 11
    case 'Q' => 12
    case 'K' => 13
    case 'A' => 14

  def cardValueP2(card: Char): Int = card match
    case 'J' => 1
    case '2' => 2
    case '3' => 3
    case '4' => 4
    case '5' => 5
    case '6' => 6
    case '7' => 7
    case '8' => 8
    case '9' => 9
    case 'T' => 10
    case 'Q' => 12
    case 'K' => 13
    case 'A' => 14

  def toHandStrength(cards: List[Char], cardValue: Char => Int): Int =
    cards.foldLeft(0) { case (value, card) =>
      value * 16 + cardValue(card)
    }

  def computeScore(hands: List[Hand], fType: List[Char] => Int, fStrength: List[Char] => Int): Long =
    hands
      .sortBy(h => (fType(h.cards), fStrength(h.cards)))
      .zipWithIndex
      .map { case (hand, rank) =>
        hand.bid * (rank + 1)
      }
      .sum

  type Input = List[Hand]

  def parse(xs: List[String]): Input =
    xs.map { case s"$cards $bid" =>
      Hand(cards.toList, bid.toLong)
    }

  def part1(input: Input) =
    val fType     = discoverSets andThen toHandType
    val fStrength = toHandStrength(_, cardValueP1)
    computeScore(input, fType, fStrength)

  def part2(input: Input) =
    val fType     = discoverSets andThen upgradeSets andThen toHandType
    val fStrength = toHandStrength(_, cardValueP2)
    computeScore(input, fType, fStrength)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
