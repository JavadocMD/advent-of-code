package aoc2019

import aoc._
import scala.annotation.tailrec
import fastparse._
import SingleLineWhitespace._

import aoc.Day
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

object Day14 extends Day:

  case class Term(quantity: Long, chemical: String)
  case class Reaction(reagents: Seq[Term], product: Term):
    // Complexity is the number of non-ore reagents.
    val complexity = reagents.count(x => x.chemical != "ORE")
  type Chems = Map[String, Long]

  def parseInt[$: P]  = P(CharIn("0-9").rep.!).map(_.toLong)
  def parseChem[$: P] = P(CharIn("A-Z").rep.!)
  def parseTerm[$: P] = P(parseInt ~ parseChem).map({ case (q, c) => Term(q, c) })
  def parseReaction[$: P] = P(parseTerm.rep(sep = ",") ~ "=>" ~ parseTerm)
    .map({ case (rs, p) => Reaction(rs, p) })

  type Input = Seq[Reaction]
  def parse(xs: Array[String]): Input = xs.view
    .map(s => fastparse.parse(s, parseReaction(_)).get.value)
    .toSeq
    .sortBy(_.complexity)(Ordering.Int.reverse)

  /** Update our stock working backwards from product to ingredients with the given reaction. */
  def react(need: Chems, have: Chems, rx: Reaction): (Chems, Chems) =
    val qNeed = need.getOrElse(rx.product.chemical, 0L)
    val qHave = have.getOrElse(rx.product.chemical, 0L)

    // How many times do we have to duplicate the reaction to make at least enough product?
    val qProd  = rx.product.quantity
    val diff   = Math.max(0, qNeed - qHave)
    val factor = (diff / qProd) + (if (diff % qProd != 0) 1L else 0L)

    // Next: we have just the leftovers.
    val qLeftover = qProd * factor + qHave - qNeed
    val nextHave  = have.updated(rx.product.chemical, qLeftover)

    // Next: remove product from needs and add all reaction ingredients.
    val nextNeed = rx.reagents.foldLeft(
      need.removed(rx.product.chemical)
    )({
      case (m, Term(q, c)) => {
        val curr = m.getOrElse(c, 0L)
        m.updated(c, curr + q * factor)
      }
    })

    (nextNeed, nextHave)

  /** Use the given reactions to reduce our set of needs to ORE only. */
  @tailrec
  final def reduceNeeds(rxs: Seq[Reaction], need: Chems, have: Chems): (Chems, Chems) =
    if (need.size == 1 && need.contains("ORE")) (need, have)
    else {
      val rx                   = rxs.find(x => need.contains(x.product.chemical)).get
      val (nextNeed, nextHave) = react(need, have, rx)
      reduceNeeds(rxs, nextNeed, nextHave)
    }

  /** What's the ore cost of producing 1 fuel? */
  def oreCost(rxs: Seq[Reaction]): Long =
    val (need, have) = reduceNeeds(rxs, Map("FUEL" -> 1), Map())
    need("ORE")

  /** How much fuel can we make for the given amount of ore? */
  def untilCost(rxs: Seq[Reaction], maxCost: Long): Long =
    // Brute force is reasonably fast if we go by large steps:
    // calculate 10,000 fuel at a time until we go over,
    // then step back and calculate 1,000 fuel until we go over,
    // and so on down to 1.

    @tailrec
    def recurse(step: Long, need: Chems, have: Chems, fuel: Long): Long = {
      val (need2, have2) = reduceNeeds(
        rxs,
        need.updated("FUEL", step),
        have
      )
      val oreCost = need2.getOrElse("ORE", 0L)
      // No cost overrun: run it again and accumulate fuel.
      if (oreCost <= maxCost) recurse(step, need2, have2, fuel + step)
      // Cost overrun with large step: downshift and redo from prev state.
      else if (step > 1) recurse(step / 10, need, have, fuel)
      // Already at step size of 1: done!
      else fuel
    }

    recurse(100000L, Map(), Map(), 0)

  def part1(input: Input): Long = oreCost(input)

  def part2(input: Input): Long = untilCost(input, 1000000000000L)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
