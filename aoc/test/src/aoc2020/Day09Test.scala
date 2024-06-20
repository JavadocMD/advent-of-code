package aoc2020

import utest._

object Day09Test extends TestSuite:
  import Day09._

  val input = Seq(35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576)
    .map(_.toLong)

  val tests = Tests {
    "find the first invalid number" - {
      findFirstInvalid(input, 5) ==> Some(127)
    }

    "find the contiguous set" - {
      findContiguousSet(input, 127) ==> Some((2, 5))
    }
  }
