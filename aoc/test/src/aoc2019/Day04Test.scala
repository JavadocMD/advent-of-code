package aoc2019

import utest._

class Day04Test extends TestSuite:
  import Day04._

  val tests = Tests {
    "detect doubles" - {
      hasDouble(1, 1, 1, 1, 1, 1) ==> true
      hasDouble(1, 2, 3, 7, 8, 9) ==> false
    }

    "detect limited doubles" - {
      hasLimitedDouble(1, 1, 2, 2, 3, 3) ==> true
      hasLimitedDouble(1, 2, 3, 4, 4, 4) ==> false
      hasLimitedDouble(1, 1, 1, 1, 2, 2) ==> true
    }
  }
