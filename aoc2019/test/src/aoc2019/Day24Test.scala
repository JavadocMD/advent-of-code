package aoc2019

import utest._

object Day24Test extends TestSuite:
  import Day24._

  val tests = Tests {
    "calc biodiversity" - {
      val s = "...............#.....#..."
      parse1(s) ==> 2129920
    }

    "evolve" - {
      val s0 = parse1("....##..#.#..##..#..#....")
      val s1 = parse1("#..#.####.###.###.##.##..")
      val s2 = parse1("#####....#....#...#.#.###")
      val s3 = parse1("#....####....###.##..##.#")
      val s4 = parse1("####.....###..#.....##...")

      val evolve1 = evolve(25, neighborMask1) _
      evolve1(s0) ==> s1
      evolve1(s1) ==> s2
      evolve1(s2) ==> s3
      evolve1(s3) ==> s4
    }

    "pack part 2 correctly" - {
      val s = "....##..#.#.?##..#..#...."
      // HNRLGISQABCDEJOTYXWVUPKF
      // 011001000000101000001011
      parse2(s) ==> b("011001000000101000001011")
    }

    "calc bits above" - {
      // HNRL ABCDEJOTYXWVUPKF HNRLGISQABCDEJOTYXWVUPKF
      // 1111 0000000000000000 000000000000000000000000
      val above = b("111111111111111111111111")
      val res0  = bitsAbove(above)
      res0.toBinaryString ==> "11110000000000000000000000000000000000000000"
    }

    "calc bits below" - {
      // HNRL ABCDEJOTYXWVUPKF HNRLGISQABCDEJOTYXWVUPKF
      // 0000 1111111111111111 000000000000000000000000
      val below = b("111111111111111111111111")
      val res0  = bitsBelow(below)
      res0.toBinaryString ==> "1111111111111111000000000000000000000000"
    }

    "do the part 2 example" - {
      val s = "....##..#.#.?##..#..#...."
      part2(s, 10) ==> 99
    }
  }
