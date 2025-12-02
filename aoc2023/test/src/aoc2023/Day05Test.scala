package aoc2023

import utest._

import Day05._

object Day05Test extends TestSuite:

  val tests = Tests {
    "AlmanacSection should convert values" - {
      val m = AlmanacSection(List(AlmanacMap(50, 98, 2)))
      m.convert(97) ==> 97
      m.convert(98) ==> 50
      m.convert(99) ==> 51
      m.convert(100) ==> 100
    }
  }
