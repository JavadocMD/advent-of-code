package aoc2019

import utest._

object Day06Test extends TestSuite:
  import Day06._

  val testData1 = """
      |COM)B
      |B)C
      |C)D
      |D)E
      |E)F
      |B)G
      |G)H
      |D)I
      |E)J
      |J)K
      |K)L""".stripMargin.trim

  val testData2 = """
      |COM)B
      |B)C
      |C)D
      |D)E
      |E)F
      |B)G
      |G)H
      |D)I
      |E)J
      |J)K
      |K)L
      |K)YOU
      |I)SAN""".stripMargin.trim

  val tests = Tests {
    "count orbits in test data" - {
      val orbits = parse(testData1.linesIterator.toArray)
      val bodies = Day06.assembleBodies(orbits)
      Day06.countOrbits(bodies) ==> 42
    }

    "find common prefixes" - {
      val a = List(5, 4, 3, 2, 1)
      val b = List(7, 3, 2, 1)
      Day06.commonSuffix(a, b) ==> List(3, 2, 1)
    }

    "count transfers in test data" - {
      val orbits = parse(testData2.linesIterator.toArray)
      val bodies = Day06.assembleBodies(orbits)
      Day06.countTransfers(bodies) ==> Some(4)
    }
  }
