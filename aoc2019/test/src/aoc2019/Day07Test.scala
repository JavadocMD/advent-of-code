package aoc2019

import utest._

object Day07Test extends TestSuite:

  def d(s: String): Day07.Input = Day07.parse(Array(s))

  val prg1 = d("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
  val prg2 = d("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")
  val prg3 = d("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
  val prg4 = d("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
  val prg5 = d(
    "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
  )

  val tests = Tests {
    "calculate example 1" - {
      val phases = Seq[Long](4, 3, 2, 1, 0)
      Day07.runSequence(prg1, phases) ==> 43210
    }

    "calculate example 2" - {
      val phases = Seq[Long](0, 1, 2, 3, 4)
      Day07.runSequence(prg2, phases) ==> 54321
    }

    "calculate example 3" - {
      val phases = Seq[Long](1, 0, 4, 3, 2)
      Day07.runSequence(prg3, phases) ==> 65210
    }

    "find best for example 1" - {
      val res = Day07.findBestSequence(prg1)
      res ==> (Seq(4, 3, 2, 1, 0), 43210)
    }

    "find best for example 2" - {
      Day07.findBestSequence(prg2) ==> (Seq(0, 1, 2, 3, 4), 54321)
    }

    "find best for example 3" - {
      Day07.findBestSequence(prg3) ==> (Seq(1, 0, 4, 3, 2), 65210)
    }

    "calculate example 4" - {
      val phases = Seq[Long](9, 8, 7, 6, 5)
      Day07.runFeedback(prg4, phases) ==> 139629729
    }

    "calculate example 5" - {
      val phases = Seq[Long](9, 7, 8, 5, 6)
      Day07.runFeedback(prg5, phases) ==> 18216
    }

    "find best for example 4" - {
      Day07.findBestFeedback(prg4) ==> (Seq(9, 8, 7, 6, 5), 139629729L)
    }

    "find best for example 5" - {
      Day07.findBestFeedback(prg5) ==> (Seq(9, 7, 8, 5, 6), 18216L)
    }
  }
