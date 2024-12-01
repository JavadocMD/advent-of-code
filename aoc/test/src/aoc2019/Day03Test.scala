package aoc2019

import utest._

object Day03Test extends TestSuite:
  import Day03._

  val tests = Tests {
    "parse wires" - {
      parse(Array("R8,U5,L5,D3", "U7,R6,D4,L4")) ==>
        WireRuns(
          List(
            Run(Right, 8),
            Run(Up, 5),
            Run(Left, 5),
            Run(Down, 3)
          ),
          List(
            Run(Up, 7),
            Run(Right, 6),
            Run(Down, 4),
            Run(Left, 4)
          )
        )
    }

    object Examples:
      lazy val zero = parse(Array("R8,U5,L5,D3", "U7,R6,D4,L4"))
      lazy val one  = parse(Array("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"))
      lazy val two = parse(Array("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))

    "extend points" - {
      Segment.from(Examples.zero.a, Point.zero) ==> List(
        Segment(Point(0, 0), Point(8, 0), 0),
        Segment(Point(8, 0), Point(8, 5), 8),
        Segment(Point(8, 5), Point(3, 5), 13),
        Segment(Point(3, 5), Point(3, 2), 18)
      )
    }

    "detect intersections" - {
      Day03.intersect(
        Segment(Point(0, 0), Point(1, 0), 0),
        Segment(Point(0, 0), Point(0, 1), 0)
      ) ==> Some(Point(0, 0))
      Day03.intersect(
        Segment(Point(0, 0), Point(1, 0), 0),
        Segment(Point(0, 1), Point(1, 1), 0)
      ) ==> None
    }

    "detect intersections between wire runs" - {
      val a  = Segment.from(Examples.zero.a, Point.zero)
      val b  = Segment.from(Examples.zero.b, Point.zero)
      val xs = Day03.intersectAll(a, b).map(i => i.point)
      xs.contains(Point(0, 0)) ==> true
      xs.contains(Point(6, 5)) ==> true
      xs.contains(Point(3, 3)) ==> true
    }

    // Part 1

    "discover nearest intersection: example 0" - {
      nearestIntersection(Examples.zero) ==> Some(6)
    }

    "discover nearest intersection: example 1" - {
      nearestIntersection(Examples.one) ==> Some(159)
    }

    "discover nearest intersection: example 2" - {
      nearestIntersection(Examples.two) ==> Some(135)
    }

    // Part 2

    "discover shortest intersection: example 0" - {
      shortestIntersection(Examples.zero) ==> Some(30)
    }

    "discover shortest intersection: example 1" - {
      shortestIntersection(Examples.one) ==> Some(610)
    }

    "discover shortest intersection: example 2" - {
      shortestIntersection(Examples.two) ==> Some(410)
    }
  }
