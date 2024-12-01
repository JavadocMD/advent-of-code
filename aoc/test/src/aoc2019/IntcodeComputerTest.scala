package aoc2019

import utest._

object IntcodeComputerTest extends TestSuite:
  import IntcodeComputer.State

  val m          = IntcodeComputer.parseMemory _
  val emptyInput = Seq[Long]().iterator

  val tests = Tests {
    "run test 00" - {
      val prg  = m("1,9,10,3,2,3,11,0,99,30,40,50")
      val exp  = m("3500,9,10,70,2,3,11,0,99,30,40,50")
      val init = State(prg, emptyInput)
      IntcodeComputer.run(init).memory ==> exp
    }

    "run test 01" - {
      val prg  = m("1,0,0,0,99")
      val exp  = m("2,0,0,0,99")
      val init = State(prg, emptyInput)
      IntcodeComputer.run(init).memory ==> exp
    }

    "run test 02" - {
      val prg  = m("2,3,0,3,99")
      val exp  = m("2,3,0,6,99")
      val init = State(prg, emptyInput)
      IntcodeComputer.run(init).memory ==> exp
    }

    "run test 03" - {
      val prg  = m("2,4,4,5,99,0")
      val exp  = m("2,4,4,5,99,9801")
      val init = State(prg, emptyInput)
      IntcodeComputer.run(init).memory ==> exp
    }

    "run test 04" - {
      val prg  = m("1,1,1,4,99,5,6,0,99")
      val exp  = m("30,1,1,4,2,5,6,0,99")
      val init = State(prg, emptyInput)
      IntcodeComputer.run(init).memory ==> exp
    }

    "run test 05" - {
      val prg  = m("3,0,4,0,99")
      val ins  = m("1")
      val exp  = m("1")
      val init = State(prg, ins.iterator)
      IntcodeComputer.run(init).output ==> exp
    }

    "run test 06" - {
      val prg  = m("1002,4,3,4,33")
      val exp  = m("1002,4,3,4,99")
      val init = State(prg, emptyInput)
      IntcodeComputer.run(init).memory ==> exp
    }

    /** Repeatedly run a program which expects 1 input over a sequence of inputs and return the corresponding sequence
      * of outputs.
      */
    def testProgramIO1(prg: IndexedSeq[Long], ins: Seq[Long]): Seq[Long] = {
      val res = for {
        i <- ins
        state = State(prg, Iterator.single(i))
      } yield IntcodeComputer.run(state).output
      res.flatten
    }

    "run test 07" - {
      val prg = m("3,9,8,9,10,9,4,9,99,-1,8")
      val ins = m("1,2,9,-12,8,8")
      val exp = m("0,0,0,0,1,1")
      testProgramIO1(prg, ins) ==> exp
    }

    "run test 08" - {
      val prg = m("3,9,7,9,10,9,4,9,99,-1,8")
      val ins = m("1,2,9,-12,8,8")
      val exp = m("1,1,0,1,0,0")
      testProgramIO1(prg, ins) ==> exp
    }

    "run test 09" - {
      val prg = m("3,3,1108,-1,8,3,4,3,99")
      val ins = m("1,2,9,-12,8,8")
      val exp = m("0,0,0,0,1,1")
      testProgramIO1(prg, ins) ==> exp
    }

    "run test 10" - {
      val prg = m("3,3,1107,-1,8,3,4,3,99")
      val ins = m("1,2,9,-12,8,8")
      val exp = m("1,1,0,1,0,0")
      testProgramIO1(prg, ins) ==> exp
    }

    "run test 11" - {
      val prg = m("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")
      val ins = m("1,0,9,-12,0,8")
      val exp = m("1,0,1,1,0,1")
      testProgramIO1(prg, ins) ==> exp
    }

    "run test 12" - {
      val prg = m("3,3,1105,-1,9,1101,0,0,12,4,12,99,1")
      val ins = m("1,0,9,-12,0,8")
      val exp = m("1,0,1,1,0,1")
      testProgramIO1(prg, ins) ==> exp
    }

    "run test 13" - {
      val prg = m(
        "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31," +
          "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104," +
          "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
      )
      val ins = m("1,2,9,-12,81923,8,8")
      val exp = m("999,999,1001,999,1001,1000,1000")
      testProgramIO1(prg, ins) ==> exp
    }

    "run test 14" - {
      val prg  = m("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
      val exp  = prg
      val init = State(prg, emptyInput)
      IntcodeComputer.run(init).output ==> exp
    }

    "run test 15" - {
      val prg  = m("1102,34915192,34915192,7,4,7,99,0")
      val exp  = prg
      val init = State(prg, emptyInput)
      IntcodeComputer.run(init).output.head.toString.size ==> 16
    }

    "run test 16" - {
      val prg  = m("104,1125899906842624,99")
      val exp  = prg
      val init = State(prg, emptyInput)
      IntcodeComputer.run(init).output ==> Seq(1125899906842624L)
    }
  }
