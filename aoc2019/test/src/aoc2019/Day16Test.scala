package aoc2019

import utest._

object Day16Test extends TestSuite:
  import Day16._

  def p(s: String) = parse(Array(s))

  val tests = Tests {
    "calculate example phases" - {
      val res0 = p("12345678")
      val res1 = phase(res0)
      res1 ==> p("48226158")
      val res2 = phase(res1)
      res2 ==> p("34040438")
      val res3 = phase(res2)
      res3 ==> p("03415518")
      val res4 = phase(res3)
      res4 ==> p("01029498")
    }

    "calculate repeated phases" - {
      val res0 = phaseRepeat(p("12345678"), 4)
      res0 ==> p("01029498")
    }

    "calculate the longer examples" - {
      val res0 = phaseRepeat(p("80871224585914546619083218645595"), 100)
      res0.take(8) ==> p("24176176")
      val res1 = phaseRepeat(p("19617804207202209144916044189917"), 100)
      res1.take(8) ==> p("73745418")
      val res2 = phaseRepeat(p("69317163492948606335995924319873"), 100)
      res2.take(8) ==> p("52432133")
    }

    "calculate the second part examples" - {
      val inp0 = p("03036732577212944063491565474664")
      process(inp0) ==> p("84462026")
      val inp1 = p("02935109699940807407585447034323")
      process(inp1) ==> p("78725270")
      val inp2 = p("03081770884921959731165446850517")
      process(inp2) ==> p("53553731")
    }
  }
