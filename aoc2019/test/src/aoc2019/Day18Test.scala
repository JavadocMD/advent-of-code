package aoc2019

import utest._

object Day18Test extends TestSuite:
  import Day18._

  val tests = Tests {
    "solve example one" - {
      val map =
        """|#########
         |#b.A.@.a#
         |#########""".stripMargin
      val maze = parse(map.split("\n"))

      solve(maze) ==> 8
    }

    "solve example two" - {
      val map =
        """|########################
         |#f.D.E.e.C.b.A.@.a.B.c.#
         |######################.#
         |#d.....................#
         |########################""".stripMargin
      val maze = parse(map.split("\n"))

      solve(maze) ==> 86
    }

    "solve example three" - {
      val map =
        """|########################
         |#...............b.C.D.f#
         |#.######################
         |#.....@.a.B.c.d.A.e.F.g#
         |########################""".stripMargin
      val maze = parse(map.split("\n"))

      solve(maze) ==> 132
    }

    "solve example four" - {
      val map =
        """|#################
         |#i.G..c...e..H.p#
         |########.########
         |#j.A..b...f..D.o#
         |########@########
         |#k.E..a...g..B.n#
         |########.########
         |#l.F..d...h..C.m#
         |#################""".stripMargin
      val maze = parse(map.split("\n"))

      solve(maze) ==> 136
    }

    "solve example five" - {
      val map =
        """|########################
         |#@..............ac.GI.b#
         |###d#e#f################
         |###A#B#C################
         |###g#h#i################
         |########################""".stripMargin
      val maze = parse(map.split("\n"))

      solve(maze) ==> 81
    }
  }
