package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day07 extends Day:
  type Input = Array[Rule]
  def parse(xs: Array[String]): Input = xs.map(Rule.from)

  case class Bag(color: String, count: Int)
  case class Rule(outer: Bag, inner: Set[Bag])

  object Rule:
    import fastparse._
    import fastparse.NoWhitespace._
    import fastparse.Parsed.{Success, Failure}

    def word[$: P] = P(CharIn("a-z").rep)
    def num[$: P]  = P(CharIn("0-9").rep).!.map(_.toInt)
    def bag[$: P]  = P(word ~ " " ~ word).!
    def bagWithCount[$: P] =
      P(num ~ " " ~ bag ~ " bag" ~ "s".?)
        .map { case (count, color) => Bag(color, count) }

    def noBags[$: P] = P("no other bags").!.map(_ => Seq.empty[Bag])
    def rule[$: P] =
      P(Start ~ bag ~ " bags contain " ~ (noBags | bagWithCount.rep(sep = ", ")) ~ "." ~ End)
        .map { case (outer, inners) => Rule(Bag(outer, 1), inners.toSet) }

    def from(s: String): Rule = fastparse.parse(s, rule(_)) match
      case Success(r, _) => r
      case _             => throw new Exception(s"Unable to parse $s")

  def findBagsThatCanContain(color: String, rules: Array[Rule]): Set[String] =
    var result = Set.empty[String]
    // Repeatedly apply the rules to our working set until it's empty.
    var working = List(List(color))
    while (!working.isEmpty) {
      var nextWorking = List.empty[List[String]]
      for {
        p <- working
        r <- rules
        if (r.inner.exists(_.color == p.head))
      } {
        // Do any rules apply (based on inner bags)?
        // If so, expand their path by prepending the rule's outer bag.
        // Expanded paths remain in our working set.
        nextWorking ::= (r.outer.color :: p)
        // And of course the new outer bag is a valid container.
        result += r.outer.color
      }
      working = nextWorking
    }
    result

  def part1(rules: Input): Int = findBagsThatCanContain("shiny gold", rules).size

  def findBagsContainedIn(color: String, rules: Array[Rule]): Int =
    def recurse(color: String, factor: Int): Int =
      val Rule(outer, inners) = rules.find(_.outer.color == color).get
      if (inners.isEmpty) 0
      else inners.iterator.map({ case Bag(c, n) => factor * n + recurse(c, factor * n) }).sum
    recurse("shiny gold", 1)

  def part2(rules: Input): Int = findBagsContainedIn("shiny gold", rules)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
