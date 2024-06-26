package aoc2020

import utest._

object Day07Test extends TestSuite:
  import Day07._

  val input = """
    |light red bags contain 1 bright white bag, 2 muted yellow bags.
    |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
    |bright white bags contain 1 shiny gold bag.
    |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
    |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
    |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
    |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
    |faded blue bags contain no other bags.
    |dotted black bags contain no other bags.""".trim.stripMargin

  val input2 = """
    |shiny gold bags contain 2 dark red bags.
    |dark red bags contain 2 dark orange bags.
    |dark orange bags contain 2 dark yellow bags.
    |dark yellow bags contain 2 dark green bags.
    |dark green bags contain 2 dark blue bags.
    |dark blue bags contain 2 dark violet bags.
    |dark violet bags contain no other bags.""".trim.stripMargin

  val tests = Tests {
    "Day07 should find bags that can contain" - {
      val rules = input.linesIterator.map(Rule.from).toArray

      findBagsThatCanContain("shiny gold", rules) ==> Set(
        "bright white",
        "muted yellow",
        "dark orange",
        "light red"
      )
    }

    "it should find bag contained" - {
      val rules = input.linesIterator.map(Rule.from).toArray
      findBagsContainedIn("shiny gold", rules) ==> 32
    }

    "it should count bags contained" - {
      val rules = input2.linesIterator.map(Rule.from).toArray
      findBagsContainedIn("shiny gold", rules) ==> 126
    }
  }
