package aoc2022

import utest._

object Day06Test extends TestSuite:
  import Day06._
  val tests = Tests {
    "marker(4) easy success" - {
      findMarker("abcd", 4) ==> 4
    }
    "marker(4) easy failure" - {
      findMarker("aaaa", 4) ==> -1
    }
    "marker(4) too short" - {
      findMarker("aa", 4) ==> -1
    }

    "marker(4) test case 1" - {
      findMarker("bvwbjplbgvbhsrlpgdmjqwftvncz", 4) ==> 5
    }
    "marker(4) test case 2" - {
      findMarker("nppdvjthqldpwncqszvftbrmjlhg", 4) ==> 6
    }
    "marker(4) test case 3" - {
      findMarker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4) ==> 10
    }
    "marker(4) test case 4" - {
      findMarker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4) ==> 11
    }

    "marker(14) test case 1" - {
      findMarker("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14) ==> 19
    }
    "marker(14) test case 2" - {
      findMarker("bvwbjplbgvbhsrlpgdmjqwftvncz", 14) ==> 23
    }
    "marker(14) test case 3" - {
      findMarker("nppdvjthqldpwncqszvftbrmjlhg", 14) ==> 23
    }
    "marker(14) test case 4" - {
      findMarker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14) ==> 29
    }
    "marker(14) test case 5" - {
      findMarker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14) ==> 26
    }
  }
