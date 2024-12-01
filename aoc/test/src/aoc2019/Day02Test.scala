package aoc2019

import utest._

object Day02Test extends TestSuite:
  import Day02._

  val tests = Tests {
    "run test program 1" - {
      val prg = IndexedSeq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)
      val res = IndexedSeq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
      Day02.runProgram(prg) ==> res
    }

    "run test program 2" - {
      val prg = IndexedSeq(1, 0, 0, 0, 99)
      val res = IndexedSeq(2, 0, 0, 0, 99)
      Day02.runProgram(prg) ==> res
    }

    "run test program 3" - {
      val prg = IndexedSeq(2, 3, 0, 3, 99)
      val res = IndexedSeq(2, 3, 0, 6, 99)
      Day02.runProgram(prg) ==> res
    }

    "run test program 4" - {
      val prg = IndexedSeq(2, 4, 4, 5, 99, 0)
      val res = IndexedSeq(2, 4, 4, 5, 99, 9801)
      Day02.runProgram(prg) ==> res
    }

    "run test program 5" - {
      val prg = IndexedSeq(1, 1, 1, 4, 99, 5, 6, 0, 99)
      val res = IndexedSeq(30, 1, 1, 4, 2, 5, 6, 0, 99)
      Day02.runProgram(prg) ==> res
    }
  }
