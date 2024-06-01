package aoc2023

import utest._
import aoc2023.Day12._

object Day12Test extends TestSuite:
  val tests = Tests {
    "Day12 should do example 1" - {
      val r = parseRow("???.### 1,1,3")
      possible(r) ==> 1
    }

    "Day12 should do example 2" - {
      var r = parseRow(".??..??...?##. 1,1,3")
      possible(r) ==> 4
    }

    "Day12 should do example 3" - {
      var r = parseRow("?###???????? 3,2,1")
      possible(r) ==> 10
    }

    "do this" - {
      val testInputStr = """
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
""".strip.linesIterator.toList.map(_.trim)
      val rows = testInputStr.map(parseRow).map(unfoldRow)
      val poss = rows.map(possible)
      poss ==> List(1, 16384, 1, 16, 2500, 506250)
      poss.sum ==> 525152
    }
  }
