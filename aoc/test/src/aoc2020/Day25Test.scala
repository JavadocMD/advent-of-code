package aoc2020

import utest._

object Day25Test extends TestSuite:
  import Day25._

  val tests = Tests {
    "example" - {
      findLoopSize(5764801L) ==> 8
      transform(17807724L, 8) ==> 14897079L
    }
  }
