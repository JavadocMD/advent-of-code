package aoc2019

import utest._

object Day22Test extends TestSuite:
  import Day22._

  def prog(s: String) = s.stripMargin.trim.split("\n").map(Op.parse)

  val tests = Tests {
    "deal into new stack" - {
      val d = Deck.of(10)
      d.dins.cards.toList ==> List(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
    }

    "cut N positive" - {
      val d = Deck.of(10)
      d.cut(3).cards.toList ==> List(3, 4, 5, 6, 7, 8, 9, 0, 1, 2)
    }

    "cut N negative" - {
      val d = Deck.of(10)
      d.cut(-4).cards.toList ==> List(6, 7, 8, 9, 0, 1, 2, 3, 4, 5)
    }

    "deal in increment N" - {
      val d = Deck.of(10)
      d.dinc(3).cards.toList ==> List(0, 7, 4, 1, 8, 5, 2, 9, 6, 3)
    }

    "shuffle example 1" - {
      val steps = prog("""
        |deal with increment 7
        |deal into new stack
        |deal into new stack
        |""")
      val res0 = shuffle(10, steps).cards.toList
      res0 ==> List(0, 3, 6, 9, 2, 5, 8, 1, 4, 7)
    }

    "shuffle example 2" - {
      val steps = prog("""
        |cut 6
        |deal with increment 7
        |deal into new stack
        |""")
      val res0 = shuffle(10, steps).cards.toList
      res0 ==> List(3, 0, 7, 4, 1, 8, 5, 2, 9, 6)
    }

    "shuffle example 3" - {
      val steps = prog("""
        |deal with increment 7
        |deal with increment 9
        |cut -2
        |""")
      val res0 = shuffle(10, steps).cards.toList
      res0 ==> List(6, 3, 0, 7, 4, 1, 8, 5, 2, 9)
    }

    "shuffle example 4" - {
      val steps = prog("""
        |deal into new stack
        |cut -2
        |deal with increment 7
        |cut 8
        |cut -4
        |deal with increment 7
        |cut 3
        |deal with increment 9
        |deal with increment 3
        |cut -1
        |""")
      val res0 = shuffle(10, steps).cards.toList
      res0 ==> List(9, 2, 5, 8, 1, 4, 7, 0, 3, 6)
    }
  }
