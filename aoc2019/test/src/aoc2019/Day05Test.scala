package aoc2019

import utest._
import scala.concurrent.Future

object Day05Test extends TestSuite:
  import Day05._

  val test_cases = Seq(
    (IndexedSeq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50), IndexedSeq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)),
    (IndexedSeq(1, 0, 0, 0, 99), IndexedSeq(2, 0, 0, 0, 99)),
    (IndexedSeq(2, 3, 0, 3, 99), IndexedSeq(2, 3, 0, 6, 99)),
    (IndexedSeq(2, 4, 4, 5, 99, 0), IndexedSeq(2, 4, 4, 5, 99, 9801)),
    (IndexedSeq(1, 1, 1, 4, 99, 5, 6, 0, 99), IndexedSeq(30, 1, 1, 4, 2, 5, 6, 0, 99))
  )

  def input1()           = Future.successful(1)
  def outputNoop(x: Int) = {}

  val tests = Tests {
    "run old test program 1" - {
      val cmp        = new IntcodeComputer(input1, outputNoop)
      val (prg, exp) = test_cases(0)
      cmp.runProgram(prg).map(_ ==> exp)
    }

    "run old test program 2" - {
      val cmp        = new IntcodeComputer(input1, outputNoop)
      val (prg, exp) = test_cases(1)
      cmp.runProgram(prg).map(_ ==> exp)
    }

    "run old test program 3" - {
      val cmp        = new IntcodeComputer(input1, outputNoop)
      val (prg, exp) = test_cases(2)
      cmp.runProgram(prg).map(_ ==> exp)
    }

    "run old test program 4" - {
      val cmp        = new IntcodeComputer(input1, outputNoop)
      val (prg, exp) = test_cases(3)
      cmp.runProgram(prg).map(_ ==> exp)
    }

    "run old test program 5" - {
      val cmp        = new IntcodeComputer(input1, outputNoop)
      val (prg, exp) = test_cases(4)
      cmp.runProgram(prg).map(_ ==> exp)
    }

    "handle input and output" - {
      var outputs  = Vector[Int]()
      val computer = new IntcodeComputer(input1, n => outputs :+= n)
      val prg      = parse(Array("3,0,4,0,99"))
      computer.runProgram(prg).map(_ => outputs ==> Vector(1))
    }

    "run program 1" - {
      val computer = new IntcodeComputer(input1, outputNoop)
      val prg      = parse(Array("1002,4,3,4,33"))
      val exp      = parse(Array("1002,4,3,4,99"))
      computer.runProgram(prg).map(_ ==> exp)
    }

    /** Repeatedly run a program which expects 1 input over a sequence of inputs and return the corresponding sequence
      * of outputs.
      */
    def testProgramIO1(prg: IndexedSeq[Int], inputs: Seq[Int]): Future[Seq[Int]] = {
      val inputFn = inputs.map(Future.successful).iterator.next _
      var outputs = scala.collection.mutable.Buffer[Int]()
      val cmp     = new IntcodeComputer(inputFn, outputs.append)
      val z       = Future.successful(IndexedSeq[Int]())
      inputs
        .foldLeft(z)({ case (f, _) =>
          f.flatMap(_ => cmp.runProgram(prg))
        })
        .map(_ => outputs.toSeq)
    }

    "run program 2" - {
      val prg = parse(Array("3,9,8,9,10,9,4,9,99,-1,8"))
      val in  = List(1, 2, 9, -12, 8, 8)
      val exp = List(0, 0, 0, 0, 1, 1)
      testProgramIO1(prg, in).map(_ ==> exp)
    }

    "run program 3" - {
      val prg = parse(Array("3,9,7,9,10,9,4,9,99,-1,8"))
      val in  = List(1, 2, 9, -12, 8, 8)
      val exp = List(1, 1, 0, 1, 0, 0)
      testProgramIO1(prg, in).map(_ ==> exp)
    }

    "run program 4" - {
      val prg = parse(Array("3,3,1108,-1,8,3,4,3,99"))
      val in  = List(1, 2, 9, -12, 8, 8)
      val exp = List(0, 0, 0, 0, 1, 1)
      testProgramIO1(prg, in).map(_ ==> exp)
    }

    "run program 5" - {
      val prg = parse(Array("3,3,1107,-1,8,3,4,3,99"))
      val in  = List(1, 2, 9, -12, 8, 8)
      val exp = List(1, 1, 0, 1, 0, 0)
      testProgramIO1(prg, in).map(_ ==> exp)
    }

    "run program 6" - {
      val prg = parse(Array("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"))
      val in  = List(1, 0, 9, -12, 0, 8)
      val exp = List(1, 0, 1, 1, 0, 1)
      testProgramIO1(prg, in).map(_ ==> exp)
    }

    "run program 7" - {
      val prg = parse(Array("3,3,1105,-1,9,1101,0,0,12,4,12,99,1"))
      val in  = List(1, 0, 9, -12, 0, 8)
      val exp = List(1, 0, 1, 1, 0, 1)
      testProgramIO1(prg, in).map(_ ==> exp)
    }

    "run program 8" - {
      val prg = parse(
        Array(
          "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31," +
            "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104," +
            "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
        )
      )
      val in  = List(1, 2, 9, -12, 81923, 8, 8)
      val exp = List(999, 999, 1001, 999, 1001, 1000, 1000)
      testProgramIO1(prg, in).map(_ ==> exp)
    }
  }
