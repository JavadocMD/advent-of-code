package aoc2021

import scala.annotation.tailrec

import aoc.Day

object Day21 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  case class Game(space1: Int, space2: Int, score1: Int, score2: Int):
    def get(player: Int): (Int, Int) =
      if player == 1 then (space1, score1) else (space2, score2)
    def set(player: Int, space: Int, score: Int): Game =
      if player == 1 then this.copy(space1 = space, score1 = score)
      else this.copy(space2 = space, score2 = score)

  lazy val d100: LazyList[Int] = LazyList.range(1, 101) #::: d100

  @tailrec
  def play1(game: Game, die: Iterator[Int], turns: Int = 1): (Game, Int) =
    val player = turns % 2

    val (sp, sc) = game.get(player)
    val roll     = die.next + die.next + die.next
    val space    = (sp - 1 + roll) % 10 + 1
    val score    = sc + space
    val next     = game.set(player, space, score)
    if score >= 1000 then (next, turns)
    else play1(next, die, turns + 1)

  def part1(input: Input) =
    val initial       = Game(9, 3, 0, 0)
    val (game, turns) = play1(initial, d100.iterator)
    Math.min(game.score1, game.score2) * turns * 3

  // (roll x3 result sum, how many universes this result occurs)
  val outcomes = Seq(1, 1, 1, 2, 2, 2, 3, 3, 3)
    .combinations(3)
    .flatMap(_.permutations)
    .map(_.sum)
    .toSeq
    .groupBy(identity)
    .view
    .mapValues(_.size)
    .toSeq

  case class Multiverse(active: Map[Game, Long], wins1: Long, wins2: Long):
    def get(player: Int): Long = if player == 1 then wins1 else wins2
    def set(active: Map[Game, Long], player: Int, wins: Long): Multiverse =
      if player == 1 then this.copy(active, wins1 = wins)
      else this.copy(active, wins2 = wins)

  @tailrec
  def play2(multiverse: Multiverse, turns: Int = 1): Multiverse =
    val player = turns % 2
    if multiverse.active.isEmpty then multiverse
    else
      var wins   = multiverse.get(player)
      var active = Map.empty[Game, Long]
      for
        (game, gameUnis) <- multiverse.active
        (roll, rollUnis) <- outcomes
      do
        val (currSpace, currScore) = game.get(player)

        val space = (currSpace - 1 + roll) % 10 + 1
        val score = currScore + space
        if (score >= 21) wins += gameUnis * rollUnis
        else
          val key  = game.set(player, space, score)
          val unis = active.getOrElse(key, 0L)
          active = active.updated(key, unis + gameUnis * rollUnis)
      play2(multiverse.set(active, player, wins), turns + 1)

  def part2(input: Input) =
    val initial = Multiverse(Map(Game(9, 3, 0, 0) -> 1L), 0L, 0L)
    val result  = play2(initial)
    Math.max(result.wins1, result.wins2)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
