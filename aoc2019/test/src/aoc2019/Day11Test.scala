package aoc2019

import utest._

object Day11Test extends TestSuite:
  import Day11._

  val p = Painter.blank
    .move(White, Left)
    .move(Black, Left)
    .move(White, Left)
    .move(White, Left)
    .move(Black, Right)
    .move(White, Left)
    .move(White, Left)

  val tests = Tests {
    "track the painter example" - {
      p.painted.size ==> 6
      p.position ==> Vector(0, -1)
      p.facing ==> West
      p.currentPaint ==> Black
    }

    "draw the painter example" - {
      draw(p) ==> "..#\n..#\n##."
    }
  }
