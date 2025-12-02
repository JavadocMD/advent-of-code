package aoc2020

import utest._

object Day17Test extends TestSuite:
  import Day17._

  val example = Array(
    ".#.",
    "..#",
    "###"
  )

  val tests = Tests {
    "parse input" - {
      val active0 = parse(example)
      active0.size ==> 5
      assert(active0.contains(Vector3(1, 0, 0)))
      assert(active0.contains(Vector3(2, 1, 0)))
      assert(active0.contains(Vector3(0, 2, 0)))
      assert(active0.contains(Vector3(1, 2, 0)))
      assert(active0.contains(Vector3(2, 2, 0)))
    }

    "evolve 3d example (one cycle)" - {
      val active0 = parse(example)
      val active1 = evolve3d(active0)
      active1.size ==> 11

      assert(active1.contains(Vector3(0, 1, -1)))
      assert(active1.contains(Vector3(1, 3, -1)))
      assert(active1.contains(Vector3(2, 2, -1)))

      assert(active1.contains(Vector3(0, 1, 0)))
      assert(active1.contains(Vector3(2, 1, 0)))
      assert(active1.contains(Vector3(1, 2, 0)))
      assert(active1.contains(Vector3(2, 2, 0)))
      assert(active1.contains(Vector3(1, 3, 0)))

      assert(active1.contains(Vector3(0, 1, 1)))
      assert(active1.contains(Vector3(1, 3, 1)))
      assert(active1.contains(Vector3(2, 2, 1)))
    }

    "evolve 3d example (six cycles)" - {
      part1(parse(example)) ==> 112
    }
  }
