package aoc2019

import utest._

object Day10Test extends TestSuite:
  import Day10._

  val πof4 = Math.PI / 4d

  extension (self: Double)
    def =~>(other: Double): Unit =
      math.abs(self - other) < 0.000001d ==> true

  val tests = Tests {
    "calculate angles between points" - {
      import Vector._
      angleBetween(Vector(0, 0), Vector(+0, -1)) =~> 0 * πof4
      angleBetween(Vector(0, 0), Vector(+1, -1)) =~> 1 * πof4
      angleBetween(Vector(0, 0), Vector(+1, +0)) =~> 2 * πof4
      angleBetween(Vector(0, 0), Vector(+1, +1)) =~> 3 * πof4
      angleBetween(Vector(0, 0), Vector(+0, +1)) =~> 4 * πof4
      angleBetween(Vector(0, 0), Vector(-1, +1)) =~> 5 * πof4
      angleBetween(Vector(0, 0), Vector(-1, +0)) =~> 6 * πof4
      angleBetween(Vector(0, 0), Vector(-1, -1)) =~> 7 * πof4
    }

    "calculate run/rise" - {
      import Vector._

      deltaBetween(Vector(0, 0), Vector(1, 1)) ==> Vector(+1, +1)
      deltaBetween(Vector(0, 0), Vector(2, 2)) ==> Vector(+1, +1)
      deltaBetween(Vector(1, 1), Vector(0, 0)) ==> Vector(-1, -1)
      deltaBetween(Vector(2, 2), Vector(0, 0)) ==> Vector(-1, -1)

      deltaBetween(Vector(0, 0), Vector(0, 1)) ==> Vector(+0, +1)
      deltaBetween(Vector(0, 1), Vector(0, 0)) ==> Vector(+0, -1)
      deltaBetween(Vector(0, 0), Vector(1, 0)) ==> Vector(+1, +0)
      deltaBetween(Vector(1, 0), Vector(0, 0)) ==> Vector(-1, +0)

      deltaBetween(Vector(5, 5), Vector(7, 7)) ==> Vector(+1, +1)
      deltaBetween(Vector(5, 5), Vector(7, 3)) ==> Vector(+1, -1)
      deltaBetween(Vector(5, 5), Vector(3, 7)) ==> Vector(-1, +1)
      deltaBetween(Vector(5, 5), Vector(3, 3)) ==> Vector(-1, -1)

      deltaBetween(Vector(0, 0), Vector(6, 8)) ==> Vector(+3, +4)
      deltaBetween(Vector(0, 8), Vector(6, 0)) ==> Vector(+3, -4)
      deltaBetween(Vector(6, 0), Vector(0, 8)) ==> Vector(-3, +4)
      deltaBetween(Vector(6, 8), Vector(0, 0)) ==> Vector(-3, -4)

      deltaBetween(Vector(6, 6), Vector(6, 6)) ==> Vector(+0, +0)

      deltaBetween(Vector(1, 0), Vector(0, 1)) ==> Vector(-1, +1)
    }

    val simpleMap = m"""
      |.#.
      |#..
      |###"""

    "parse simple map" - {
      val m = parse(simpleMap)

      m.size ==> Vector(3, 3)

      m.isAsteroid(Vector(0, 0)) ==> false
      m.isAsteroid(Vector(1, 0)) ==> true
      m.isAsteroid(Vector(2, 0)) ==> false

      m.isAsteroid(Vector(0, 1)) ==> true
      m.isAsteroid(Vector(1, 1)) ==> false
      m.isAsteroid(Vector(2, 1)) ==> false

      m.isAsteroid(Vector(0, 2)) ==> true
      m.isAsteroid(Vector(1, 2)) ==> true
      m.isAsteroid(Vector(2, 2)) ==> true
    }

    "determine asteroid visibility" - {
      val m = parse(simpleMap)

      m.canSee(Vector(1, 0), Vector(0, 1)) ==> true
      m.canSee(Vector(1, 0), Vector(0, 2)) ==> true
      m.canSee(Vector(0, 2), Vector(1, 2)) ==> true
      m.canSee(Vector(0, 2), Vector(2, 2)) ==> false

      m.countVisible(Vector(0, 2)) ==> 3
    }

    "process example 1" - {
      val example = m"""
        |.#..#
        |.....
        |#####
        |....#
        |...##"""
      val m = parse(example)
      m.countVisible(Vector(1, 0)) ==> 7
      m.countVisible(Vector(4, 0)) ==> 7
      m.countVisible(Vector(0, 2)) ==> 6
      m.countVisible(Vector(1, 2)) ==> 7
      m.countVisible(Vector(2, 2)) ==> 7
      m.countVisible(Vector(3, 2)) ==> 7
      m.countVisible(Vector(4, 2)) ==> 5
      m.countVisible(Vector(4, 3)) ==> 7
      m.countVisible(Vector(3, 4)) ==> 8
      m.countVisible(Vector(4, 4)) ==> 7
      bestMonitor(m) ==> (Vector(3, 4), 8)
    }

    "process example 2" - {
      val example = m"""
        |......#.#.
        |#..#.#....
        |..#######.
        |.#.#.###..
        |.#..#.....
        |..#....#.#
        |#..#....#.
        |.##.#..###
        |##...#..#.
        |.#....####"""
      val m = parse(example)
      bestMonitor(m) ==> (Vector(5, 8), 33)
    }

    "process example 3" - {
      val example = m"""
        |#.#...#.#.
        |.###....#.
        |.#....#...
        |##.#.#.#.#
        |....#.#.#.
        |.##..###.#
        |..#...##..
        |..##....##
        |......#...
        |.####.###."""
      val m = parse(example)
      bestMonitor(m) ==> (Vector(1, 2), 35)
    }

    "process example 4" - {
      val example = m"""
        |.#..#..###
        |####.###.#
        |....###.#.
        |..###.##.#
        |##.##.#.#.
        |....###..#
        |..#.#..#.#
        |#..#.#.###
        |.##...##.#
        |.....#.#.."""
      val m = parse(example)
      bestMonitor(m) ==> (Vector(6, 3), 41)
    }

    val largeExample = m"""
      |.#..##.###...#######
      |##.############..##.
      |.#.######.########.#
      |.###.#######.####.#.
      |#####.##.#.##.###.##
      |..#####..#.#########
      |####################
      |#.####....###.#.#.##
      |##.#################
      |#####.##.###..####..
      |..######..##.#######
      |####.##.####...##..#
      |.#####..#.######.###
      |##...#.##########...
      |#.##########.#######
      |.####.#.###.###.#.##
      |....##.##.###..#####
      |.#.#.###########.###
      |#.#.#.#####.####.###
      |###.##.####.##.#..##"""

    "process example 5" - {
      val m = parse(largeExample)
      bestMonitor(m) ==> (Vector(11, 13), 210)
    }

    "compare targets" - {
      val a = Target(Vector(1, 0), 0, 1)
      val b = Target(Vector(1, 0), 1, 0)

      (a compare b) < 0 ==> true

      val c = Target(Vector(1, 0), 1, 0)
      val d = Target(Vector(1, 0), 1, 1)

      (c compare d) < 0 ==> true
    }

    "determine targeting order" - {
      val example = m"""
        |.#....#####...#..
        |##...##.#####..##
        |##...#...#.#####.
        |..#.....#...###..
        |..#.#.....#....##"""
      val m  = parse(example)
      val xs = targetOrder(m, Vector(8, 3))

      // First nine
      xs.take(9).map(_.asteroid) ==> Seq(
        Vector(8, 1),
        Vector(9, 0),
        Vector(9, 1),
        Vector(10, 0),
        Vector(9, 2),
        Vector(11, 1),
        Vector(12, 1),
        Vector(11, 2),
        Vector(15, 1)
      )

      // Second nine
      xs.drop(9).take(9).map(_.asteroid) ==> Seq(
        Vector(12, 2),
        Vector(13, 2),
        Vector(14, 2),
        Vector(15, 2),
        Vector(12, 3),
        Vector(16, 4),
        Vector(15, 4),
        Vector(10, 4),
        Vector(4, 4)
      )

      // Third nine
      xs.drop(18).take(9).map(_.asteroid) ==> Seq(
        Vector(2, 4),
        Vector(2, 3),
        Vector(0, 2),
        Vector(1, 2),
        Vector(0, 1),
        Vector(1, 1),
        Vector(5, 2),
        Vector(1, 0),
        Vector(5, 1)
      )

      // Finally
      xs.drop(27).map(_.asteroid) ==> Seq(
        Vector(6, 1),
        Vector(6, 0),
        Vector(7, 0),
        Vector(8, 0),
        Vector(10, 1),
        Vector(14, 0),
        Vector(16, 1),
        Vector(13, 3),
        Vector(14, 3)
      )
    }

    "find the Nth targeted asteroids in large example" - {
      val m  = parse(largeExample)
      val xs = targetOrder(m, Vector(11, 13))
      xs(0).asteroid ==> Vector(11, 12)
      xs(1).asteroid ==> Vector(12, 1)
      xs(2).asteroid ==> Vector(12, 2)
      xs(9).asteroid ==> Vector(12, 8)
      xs(19).asteroid ==> Vector(16, 0)
      xs(49).asteroid ==> Vector(16, 9)
      xs(99).asteroid ==> Vector(10, 16)
      xs(198).asteroid ==> Vector(9, 6)
      xs(199).asteroid ==> Vector(8, 2)
      xs(200).asteroid ==> Vector(10, 9)
      xs(298).asteroid ==> Vector(11, 1)
    }
  }
