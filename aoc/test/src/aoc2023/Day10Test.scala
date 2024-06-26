package aoc2023

import utest._
import aoc2023.Day10._

object Day10Test extends TestSuite:
  val tests = Tests {
    test("Day10 part 2 should get example 1") {
      val testInputStr = """
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
...........
...........
""".strip.linesIterator.toList.map(_.trim)
      val afterStart = Some(Point(2, 1))
      val testInput  = parse(testInputStr)
      part2(testInput, afterStart) ==> 4
    }

    test("Day10 part 2 should get example 2") {
      val testInputStr = """
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
""".strip.linesIterator.toList.map(_.trim)
      val afterStart = Some(Point(12, 5))
      val testInput  = parse(testInputStr)
      part2(testInput, afterStart) ==> 8
    }

    test("Day10 part 2 should get example 3") {
      val testInputStr = """
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
""".strip.linesIterator.toList.map(_.trim)
      val afterStart = Some(Point(4, 1))
      val testInput  = parse(testInputStr)
      part2(testInput, afterStart) ==> 10
    }
  }
