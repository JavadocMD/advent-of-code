package aoc2019
import fastparse._

import utest._

object Day14Test extends TestSuite:
  import Day14._

  def r(s: String): Reaction = fastparse.parse(s, parseReaction(_)).get.value

  val tests = Tests {
    "parse reactions" - {
      r("10 ORE => 10 A") ==> Reaction(Seq(Term(10, "ORE")), Term(10, "A"))

      r("1 ORE => 1 B") ==> Reaction(Seq(Term(1, "ORE")), Term(1, "B"))

      r("7 A, 1 B => 1 C") ==> Reaction(Seq(Term(7, "A"), Term(1, "B")), Term(1, "C"))

      r("44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL") ==> Reaction(
        Seq(
          Term(44, "XJWVT"),
          Term(5, "KHKGT"),
          Term(1, "QDVJ"),
          Term(29, "NZVS"),
          Term(9, "GPVTF"),
          Term(48, "HKGWZ")
        ),
        Term(1, "FUEL")
      )
    }

    "react" - {
      val have = Map("D" -> 3L)
      val need = Map("C" -> 2L)
      val rx0  = r("7 A, 1 B => 1 C")

      val (resNeed, resHave) = react(need, have, rx0)
      resNeed ==> Map(
        "A" -> 14,
        "B" -> 2
      )
      resHave.filter({ case (k, v) => v != 0 }) ==> Map(
        "D" -> 3
      )
    }

    "calculate ore cost, example 0" - {
      val rxs = parse(
        Array(
          "10 ORE => 10 A",
          "1 ORE => 1 B",
          "7 A, 1 B => 1 C",
          "7 A, 1 C => 1 D",
          "7 A, 1 D => 1 E",
          "7 A, 1 E => 1 FUEL"
        )
      )
      oreCost(rxs) ==> 31
    }

    "calculate ore cost, example 1" - {
      val rxs = parse(
        Array(
          "9 ORE => 2 A",
          "8 ORE => 3 B",
          "7 ORE => 5 C",
          "3 A, 4 B => 1 AB",
          "5 B, 7 C => 1 BC",
          "4 C, 1 A => 1 CA",
          "2 AB, 3 BC, 4 CA => 1 FUEL"
        )
      )
      oreCost(rxs) ==> 165
    }

    "calculate ore cost, example 2" - {
      val rxs = parse(
        Array(
          "157 ORE => 5 NZVS",
          "165 ORE => 6 DCFZ",
          "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL",
          "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ",
          "179 ORE => 7 PSHF",
          "177 ORE => 5 HKGWZ",
          "7 DCFZ, 7 PSHF => 2 XJWVT",
          "165 ORE => 2 GPVTF",
          "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
        )
      )
      oreCost(rxs) ==> 13312
      untilCost(rxs, 1000000000000L) ==> 82892753
    }

    "calculate ore cost, example 3" - {
      val rxs = parse(
        Array(
          "2 V, 7 F, 2 C, 11 M => 1 S",
          "17 N, 3 J => 8 V",
          "53 S, 6 M, 46 Z, 81 H, 68 C, 25 G => 1 FUEL",
          "22 Z, 37 M => 5 F",
          "139 ORE => 4 N",
          "144 ORE => 7 J",
          "5 M, 7 R, 2 F, 2 V, 19 C => 3 H",
          "5 Z, 7 M, 9 V, 37 C => 6 G",
          "145 ORE => 6 M",
          "1 N => 8 C",
          "1 Z, 6 M => 4 R",
          "176 ORE => 6 Z"
        )
      )
      oreCost(rxs) ==> 180697
      untilCost(rxs, 1000000000000L) ==> 5586022
    }

    "calculate ore cost, example 4" - {
      val rxs = parse(
        Array(
          "171 ORE => 8 CNZTR",
          "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL",
          "114 ORE => 4 BHXH",
          "14 VRPVC => 6 BMBT",
          "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL",
          "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT",
          "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW",
          "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW",
          "5 BMBT => 4 WPTQ",
          "189 ORE => 9 KTJDG",
          "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP",
          "12 VRPVC, 27 CNZTR => 2 XDBXC",
          "15 KTJDG, 12 BHXH => 5 XCVML",
          "3 BHXH, 2 VRPVC => 7 MZWV",
          "121 ORE => 7 VRPVC",
          "7 XCVML => 6 RJRHP",
          "5 BHXH, 4 VRPVC => 5 LTCX"
        )
      )
      oreCost(rxs) ==> 2210736
      untilCost(rxs, 1000000000000L) ==> 460664
    }
  }
