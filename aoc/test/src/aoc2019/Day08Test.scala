package aoc2019

import utest._

object Day08Test extends TestSuite:
  import Day08._

  val tests = Tests {
    "load data" - {
      val data = parse(Array("123456789012"), 3 x 2)
      data.layers.map(_.toList).toList ==> List(List(1, 2, 3, 4, 5, 6), List(7, 8, 9, 0, 1, 2))
    }

    "merge layers" - {
      val data = parse(Array("0222112222120000"), 2 x 2)
      merge(data).layer.toList ==> List(0, 1, 1, 0)
    }
  }
