package aoc2020

import utest._

object Day15Test extends TestSuite:
  import Day15._

  val tests = Tests {
    "examples" - {
      play(Array(0, 3, 6), 2020) ==> 436
      play(Array(1, 3, 2), 2020) ==> 1
      play(Array(2, 1, 3), 2020) ==> 10
      play(Array(1, 2, 3), 2020) ==> 27
      play(Array(2, 3, 1), 2020) ==> 78
      play(Array(3, 2, 1), 2020) ==> 438
      play(Array(3, 1, 2), 2020) ==> 1836
    }
  }
