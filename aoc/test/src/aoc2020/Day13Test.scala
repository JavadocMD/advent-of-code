package aoc2020

import utest._

object Day13Test extends TestSuite:
  import Day13._

  val tests = Tests {
    "p2e1" - {
      Seq((7L, 0L), (13L, 1L), (59L, 4L), (31L, 6L), (19L, 7L))
        .reduceLeft(search)
        ._2 ==> 1068781L
    }

    "p2e2" - {
      Seq((17L, 0L), (13L, 2L), (19L, 3L))
        .reduceLeft(search)
        ._2 ==> 3417L
    }

    "p2e3" - {
      Seq((67L, 0L), (7L, 2L), (59L, 3L), (61L, 4L))
        .reduceLeft(search)
        ._2 ==> 779210L
    }

    "p2e4" - {
      Seq((67L, 0L), (7L, 1L), (59L, 3L), (61L, 4L))
        .reduceLeft(search)
        ._2 ==> 1261476L
    }

    "p2e5" - {
      Seq((1789L, 0L), (37L, 1L), (47L, 2L), (1889L, 3L))
        .reduceLeft(search)
        ._2 ==> 1202161486L
    }
  }
