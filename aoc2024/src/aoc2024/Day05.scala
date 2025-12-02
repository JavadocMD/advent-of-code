package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day05 extends Day:

  type Rules  = Map[Int, List[Int]]
  type Update = List[Int]
  type Input  = (Rules, List[Update])

  def parse(xs: List[String]): Input =
    val (ruleLines, updateLines) = xs.span(_ != "")

    // Rules is a map:
    // - key is the second page in the rule,
    // - value is a list of the pages that should come before key
    // (Thus any particular rule is in "value|key" order)
    val rules = ruleLines
      .map:
        case s"$a|$b" => (a.toInt, b.toInt)
      .groupBy(_._2)
      .mapValues(_.unzip._1)
      .toMap

    val updates = updateLines.tail.map: line =>
      line.split(",").toList.map(_.toInt)

    (rules, updates)

  /** Filter the rules map so it only includes pages that are in `update`. */
  def filterRules(rules: Rules, update: List[Int]): Map[Int, List[Int]] =
    Map.from(
      for (b, as) <- rules if update.contains(b)
      yield b -> as.filter(update.contains)
    )

  /** Does an update satisfy all applicable page ordering rules? */
  def isGood(rules: Rules)(update: Update): Boolean =
    val ruleSet = filterRules(rules, update)

    @tailrec
    def recurse(xs: List[Int], before: List[Int]): Boolean =
      xs match
        case Nil => true
        case head :: tail =>
          val satisfied = ruleSet.getOrElse(head, Nil).forall(before.contains)
          satisfied && recurse(tail, head :: before)

    recurse(update, Nil)

  def part1(input: Input) =
    val (rules, updates) = input
    updates
      .filter(isGood(rules))
      .map(x => x(x.size / 2))
      .sum

  /** Fix page ordering so that it satisfies all applicable ordering rules. */
  def fix(rules: Rules)(update: Update): Update =
    val ruleSet = filterRules(rules, update)

    @tailrec
    def recurse(rem: List[Int], curr: Int, acc: List[Int]): Update =
      if rem.isEmpty then acc.reverse
      else
        val page = rem(curr)
        if ruleSet(page).forall(acc.contains)
        then recurse(rem.patch(curr, Nil, 1), 0, page :: acc)
        else recurse(rem, curr + 1, acc)

    recurse(update, 0, List.empty)

  def part2(input: Input) =
    val (rules, updates) = input
    updates
      .filterNot(isGood(rules))
      .map(fix(rules))
      .map(x => x(x.size / 2))
      .sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
