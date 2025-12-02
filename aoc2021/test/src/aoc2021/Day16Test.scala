package aoc2021

import utest._

object Day16Test extends TestSuite:
  import Day16._

  val tests = Tests {
    "Day16 should parse literal" - {
      val i = "110100101111111000101000".view.map(_.asDigit).iterator
      readPacket(i) ==> (Packet.Literal(6, 4, 2021L), 21)
    }
  }
