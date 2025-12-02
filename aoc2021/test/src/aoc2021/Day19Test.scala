package aoc2021

import utest._

object Day19Test extends TestSuite:
  import Day19._

  val tests = Tests {
    "Vector.cross" - {
      Vector(2, 1, 2).cross(Vector(3, 4, 5)) ==> Vector(-3, -4, 5)
      Vector(3, 0, 4).cross(Vector(-3, 0, 3)) ==> Vector(0, -21, 0)
    }

    "Vector.dot" - {
      Vector(1, 3, -5).dot(Vector(4, -2, -1)) ==> 3
    }

    "AxisAngle.rotate" - {
      import Axis._, Angle._
      val vs = List(
        Vector(1, 2, 3),
        Vector(-1, -2, -3)
      )

      vs.map(AxisAngle(Xp, D000).rotate) ==> vs
      vs.map(AxisAngle(Xp, D090).rotate) ==> List(
        Vector(1, -3, 2),
        Vector(-1, 3, -2)
      )
      vs.map(AxisAngle(Xp, D180).rotate) ==> List(
        Vector(1, -2, -3),
        Vector(-1, 2, 3)
      )
      vs.map(AxisAngle(Xp, D270).rotate) ==> List(
        Vector(1, 3, -2),
        Vector(-1, -3, 2)
      )
    }
  }
