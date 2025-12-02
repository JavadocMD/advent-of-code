package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day22 extends Day:
  import scala.collection.immutable.Queue

  type Deck   = Queue[Int]
  type Result = (Int, Deck)

  type Input = (Deck, Deck)
  def parse(input: Array[String]): Input =
    val cs = toChunks(input)
    val p1 = Queue.from(cs(0)).drop(1).map(_.toInt)
    val p2 = Queue.from(cs(1)).drop(1).map(_.toInt)
    (p1, p2)

  @tailrec
  final def play(deck1: Deck, deck2: Deck): Result =
    val (c1, tail1) = deck1.dequeue
    val (c2, tail2) = deck2.dequeue
    val next1       = if (c1 < c2) tail1 else tail1 :+ c1 :+ c2
    val next2       = if (c1 > c2) tail2 else tail2 :+ c2 :+ c1

    if (next2.isEmpty) (1, next1)
    else if (next1.isEmpty) (2, next2)
    else play(next1, next2)

  def score(deck: Deck): Long =
    deck.reverse.lazyZip(LazyList.from(1)).map(_.toLong * _).sum

  def part1(deck1: Deck, deck2: Deck): Long =
    val winner = play(deck1, deck2)._2
    score(winner)

  type Turn  = (Deck, Deck)
  type State = (Int, Int, Deck, Deck, Set[Turn])

  @tailrec
  final def playRec(
      player1: Deck,
      player2: Deck,
      seen: Set[Turn] = Set.empty,
      stack: List[State] = List.empty
  ): Result =
    val thisTurn = (player1, player2)
    val gameWinner = {
      if (seen.contains(thisTurn)) Some(1)
      else if (player1.isEmpty) Some(2)
      else if (player2.isEmpty) Some(1)
      else None
    }
    gameWinner match {
      case Some(winner) =>
        stack match {
          case Nil =>
            // Winner of overall game!
            if (winner == 1) (1, player1) else (2, player2)
          case (c1, c2, tail1, tail2, s) :: stackTail =>
            // Winner of sub-game; give cards to winner and continue.
            playRec(
              if (winner == 2) tail1 else tail1 :+ c1 :+ c2,
              if (winner == 1) tail2 else tail2 :+ c2 :+ c1,
              s + ((tail1, tail2)),
              stackTail
            )
        }
      case None =>
        // No winner yet, keep playing.
        var (c1, tail1) = player1.dequeue
        var (c2, tail2) = player2.dequeue
        if (tail1.size >= c1 && tail2.size >= c2) {
          // Recurse into a sub-game by pushing the outer state onto the stack.
          // The win will be applied when the sub-game is declared.
          playRec(
            tail1.take(c1),
            tail2.take(c2),
            Set.empty,
            ((c1, c2, tail1, tail2, seen)) :: stack
          )
        } else {
          // No recursion -- high card wins.
          playRec(
            if (c2 > c1) tail1 else tail1 :+ c1 :+ c2,
            if (c1 > c2) tail2 else tail2 :+ c2 :+ c1,
            seen + thisTurn,
            stack
          )
        }
    }
  end playRec

  def part2(deck1: Deck, deck2: Deck): Long =
    val winner = playRec(deck1, deck2)._2
    score(winner)

  final def main(args: Array[String]): Unit =
    val (deck1, deck2) = parse(loadInput())
    solveP1(() => part1(deck1, deck2))
    solveP2(() => part2(deck1, deck2))
