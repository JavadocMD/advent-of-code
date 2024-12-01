package aoc2019

import utest._

object Day12Test extends TestSuite:
  import Day12._

  val tests = Tests {
    "calulate gravity" - {
      val a = Moon(Vector(1, 2, 3), Vector.zero)
      (a gravity a) ==> Vector.zero
      val b = Moon(Vector(3, 2, 1), Vector.zero)
      (a gravity b) ==> Vector(+1, 0, -1)
    }

    "parse vectors" - {
      val input = Seq("<x=-1, y=0, z=23>")
      parseInput(input) ==> Seq(Moon(Vector(-1, 0, 23), Vector.zero))
    }

    "parse moons" - {
      val input = Seq("pos=<x=5, y=-3, z=-1>, vel=<x=3, y=-2, z=-2>")
      parseMoons(input) ==> Seq(Moon(Vector(5, -3, -1), Vector(3, -2, -2)))
    }

    "simulate moons" - {
      val res0 = parseMoons(
        Seq(
          "pos=<x=-1, y=  0, z= 2>, vel=<x= 0, y= 0, z= 0>",
          "pos=<x= 2, y=-10, z=-7>, vel=<x= 0, y= 0, z= 0>",
          "pos=<x= 4, y= -8, z= 8>, vel=<x= 0, y= 0, z= 0>",
          "pos=<x= 3, y=  5, z=-1>, vel=<x= 0, y= 0, z= 0>"
        )
      )

      val res1 = step(res0)
      res1 ==> parseMoons(
        Seq(
          "pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>",
          "pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>",
          "pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>",
          "pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>"
        )
      )

      val res2 = step(res1)
      res2 ==> parseMoons(
        Seq(
          "pos=<x= 5, y=-3, z=-1>, vel=<x= 3, y=-2, z=-2>",
          "pos=<x= 1, y=-2, z= 2>, vel=<x=-2, y= 5, z= 6>",
          "pos=<x= 1, y=-4, z=-1>, vel=<x= 0, y= 3, z=-6>",
          "pos=<x= 1, y=-4, z= 2>, vel=<x=-1, y=-6, z= 2>"
        )
      )

      val res3  = step(res2)
      val res4  = step(res3)
      val res5  = step(res4)
      val res6  = step(res5)
      val res7  = step(res6)
      val res8  = step(res7)
      val res9  = step(res8)
      val res10 = step(res9)

      res10 ==> parseMoons(
        Seq(
          "pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>",
          "pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>",
          "pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>",
          "pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>"
        )
      )

      res10.map(_.energy) ==> Seq(36, 45, 80, 18)
    }

    "simulate moons, example 2" - {
      val res0 = parseMoons(
        Seq(
          "pos=<x= -8, y=-10, z=  0>, vel=<x=  0, y=  0, z=  0>",
          "pos=<x=  5, y=  5, z= 10>, vel=<x=  0, y=  0, z=  0>",
          "pos=<x=  2, y= -7, z=  3>, vel=<x=  0, y=  0, z=  0>",
          "pos=<x=  9, y= -8, z= -3>, vel=<x=  0, y=  0, z=  0>"
        )
      )
      val res100 = stepN(res0, 100)
      res100 ==> parseMoons(
        Seq(
          "pos=<x=  8, y=-12, z= -9>, vel=<x= -7, y=  3, z=  0>",
          "pos=<x= 13, y= 16, z= -3>, vel=<x=  3, y=-11, z= -5>",
          "pos=<x=-29, y=-11, z= -1>, vel=<x= -3, y=  7, z=  4>",
          "pos=<x= 16, y=-13, z= 23>, vel=<x=  7, y=  1, z=  1>"
        )
      )
      res100.map(_.energy) ==> Seq(290, 608, 574, 468)
    }

    "determine cycle in example 1" - {
      val moons = parseMoons(
        Seq(
          "pos=<x=-1, y=  0, z= 2>, vel=<x= 0, y= 0, z= 0>",
          "pos=<x= 2, y=-10, z=-7>, vel=<x= 0, y= 0, z= 0>",
          "pos=<x= 4, y= -8, z= 8>, vel=<x= 0, y= 0, z= 0>",
          "pos=<x= 3, y=  5, z=-1>, vel=<x= 0, y= 0, z= 0>"
        )
      )
      findCycle(moons) ==> 2772L
    }

    "determine cycle in example 2" - {
      val moons = parseMoons(
        Seq(
          "pos=<x= -8, y=-10, z=  0>, vel=<x=  0, y=  0, z=  0>",
          "pos=<x=  5, y=  5, z= 10>, vel=<x=  0, y=  0, z=  0>",
          "pos=<x=  2, y= -7, z=  3>, vel=<x=  0, y=  0, z=  0>",
          "pos=<x=  9, y= -8, z= -3>, vel=<x=  0, y=  0, z=  0>"
        )
      )
      findCycle(moons) ==> 4686774924L
    }
  }
