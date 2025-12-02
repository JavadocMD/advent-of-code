package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day19 extends Day:
  // Resources and Robots
  case class Rs(ore: Int, clay: Int, obsidian: Int, geode: Int):
    def <=(that: Rs): Boolean =
      ore <= that.ore && clay <= that.clay && obsidian <= that.obsidian && geode <= that.geode
    def +(that: Rs): Rs =
      Rs(ore + that.ore, clay + that.clay, obsidian + that.obsidian, geode + that.geode)
    def -(that: Rs): Rs =
      Rs(ore - that.ore, clay - that.clay, obsidian - that.obsidian, geode - that.geode)

  type RecipeName = String
  case class Recipe(name: RecipeName, costs: Rs, builds: Rs)
  val noop = Recipe("noop", Rs(0, 0, 0, 0), Rs(0, 0, 0, 0))

  case class Blueprint(id: Int, recipes: List[Recipe], goc: Int, occ: Int)

  type Input = List[Blueprint]
  def parse(xs: Iterator[String]): Input = xs.map {
    case s"Blueprint ${IntP(id)}: Each ore robot costs ${IntP(oreOreCost)} ore. Each clay robot costs ${IntP(clayOreCost)} ore. Each obsidian robot costs ${IntP(
            obsidianOreCost
          )} ore and ${IntP(obsidianClayCost)} clay. Each geode robot costs ${IntP(geodeOreCost)} ore and ${IntP(geodeObsidianCost)} obsidian." =>
      val ore      = Recipe("ore", Rs(oreOreCost, 0, 0, 0), Rs(1, 0, 0, 0))
      val clay     = Recipe("clay", Rs(clayOreCost, 0, 0, 0), Rs(0, 1, 0, 0))
      val obsidian = Recipe("obsidian", Rs(obsidianOreCost, obsidianClayCost, 0, 0), Rs(0, 0, 1, 0))
      val geode    = Recipe("geode", Rs(geodeOreCost, 0, geodeObsidianCost, 0), Rs(0, 0, 0, 1))
      Blueprint(id, List(geode, obsidian, clay, ore, noop), geodeObsidianCost, obsidianClayCost)
  }.toList

  def tri(n: Int) = (n * (n + 1)) / 2

  @tailrec
  def opportunity(maxBuilt: Int, timeLeft: Int, built: Int = 0, score: Int = 0): Int =
    if timeLeft == 0 then score
    else opportunity(maxBuilt, timeLeft - 1, math.min(maxBuilt, built + 1), score + built)

  case class State(bp: Blueprint, robots: Rs, resources: Rs, timeLeft: Int):
    def maxClay     = resources.clay + (robots.clay * timeLeft) + tri(timeLeft - 1)
    def maxObsidian = resources.obsidian + (robots.obsidian * timeLeft) + opportunity(maxClay / bp.occ, timeLeft)

    def minScore = resources.geode + (robots.geode * timeLeft)
    def maxScore = minScore + opportunity(maxObsidian / bp.goc, timeLeft)

    def available: List[Recipe] =
      if timeLeft == 1 then noop :: Nil
      else bp.recipes.filter(_.costs <= resources)

    def next(op: Recipe) = State(bp, robots + op.builds, resources - op.costs + robots, timeLeft - 1)
  object State:
    def initial(bp: Blueprint, time: Int) = State(bp, Rs(1, 0, 0, 0), Rs(0, 0, 0, 0), time)
  end State

  def findBest(initial: State): Int =
    @tailrec def recurse(frontier: List[State], best: Int): Int = frontier match
      case Nil                                   => best
      case head :: tail if head.maxScore <= best => recurse(tail, best)
      case head :: tail =>
        val next     = head.available.map(head.next)
        val nextBest = next.foldLeft(0)((acc, next) => math.max(acc, next.minScore))
        recurse(next ::: tail, math.max(nextBest, best))
    recurse(initial :: Nil, 0)

  def part1(input: Input) =
    input.map { bp =>
      val score = findBest(State.initial(bp, 24))
      println(score)
      bp.id * score
    }.sum

  def part2(input: Input) =
    input
      .take(3)
      .map { bp =>
        val score = findBest(State.initial(bp, 32))
        println(score)
        score
      }
      .product

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
