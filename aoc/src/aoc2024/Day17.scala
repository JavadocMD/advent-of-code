package aoc2024

import scala.annotation.tailrec
import aoc.Day

object Day17 extends Day:

  type Input = Computer

  def parse(xs: List[String]): Input =
    xs match
      case List(
            s"Register A: $a",
            s"Register B: $b",
            s"Register C: $c",
            "",
            s"Program: $prg",
          ) =>
        Computer(
          program = prg.split(",").toIndexedSeq.map(_.toInt),
          regA = a.toLong,
          regB = b.toLong,
          regC = c.toLong,
        )
      case _ => throw Exception("Unable to parse input.")

  case class Computer(
      val program: IndexedSeq[Int],
      val pointer: Int = 0,
      val regA: Long,
      val regB: Long,
      val regC: Long,
      output: Vector[Long] = Vector.empty,
  ):
    def readCombo(operand: Int): Long =
      operand match
        case 0 | 1 | 2 | 3 => operand.toLong
        case 4             => regA
        case 5             => regB
        case 6             => regC
        case x             => throw Exception(s"Invalid combo operand: $x")

    @tailrec
    final def run: Computer =
      if pointer >= program.size then this
      else
        val opcode  = program(pointer)
        val operand = program(pointer + 1)
        val next = opcode match
          case 0 => // adv
            this.copy(
              regA = regA / math.pow(2, readCombo(operand)).toLong,
              pointer = pointer + 2,
            )
          case 1 => // bxl
            this.copy(
              regB = regB ^ operand.toLong,
              pointer = pointer + 2,
            )
          case 2 => // bst
            this.copy(
              regB = readCombo(operand) % 8,
              pointer = pointer + 2,
            )
          case 3 => // jnz
            if regA == 0 then this.copy(pointer = pointer + 2)
            else this.copy(pointer = operand)
          case 4 => // bxc
            this.copy(
              regB = regB ^ regC,
              pointer = pointer + 2,
            )
          case 5 => // out
            this.copy(
              output = output :+ readCombo(operand) % 8,
              pointer = pointer + 2,
            )
          case 6 => // bdv
            this.copy(
              regB = regA / math.pow(2, readCombo(operand)).toLong,
              pointer = pointer + 2,
            )
          case 7 => // cdv
            this.copy(
              regC = regA / math.pow(2, readCombo(operand)).toLong,
              pointer = pointer + 2,
            )
        next.run

  def part1(input: Input) = input.run.output.mkString(",")

  // Let's manually "decompile" the program:
  //
  // 2,4: bst regA         // regB = regA % 8
  // 1,2: bxl 2            // regB = regB ^ 2
  // 7,5: cdv regB         // regC = regA / (2 ** regB)
  // 1,3: bxl 3            // regB = regB ^ 3
  // 4,4: bxc _            // regB = regB ^ regC
  // 5,5: out regB         // --> regB % 8
  // 0,3: adv 3            // regA = regA / 8
  // 3,0: jnz              // if regA not 0, goto 0
  //
  // Observations:
  // - program is very linear; a sequence of instructions then a conditional jump back to 0,
  //   producing one output per loop
  // - regA is divided by 8 in each iteration, no other assignment;
  //   this means its previous value could have been any of the values from a*8 until (a*8)+8
  // - the values of regB and regC are set from regA during the loop,
  //   that is, their values from the previous loop don't matter
  // - if we know the value regA has at the end of the loop, we know that loop's output too
  // - in other words: we can try the possible values of regA until we produce the output we want!
  // - but different values of regA might produce the same output, so we will need to track multiple forks
  // - our program only has 12 outputs, though, so we shouldn't blow the stack doing simple recursion
  // - in order to halt, regA has to be 0 in the final iteration

  def loopOutput(regA: Long): Long =
    // if regA ends the loop with the given value,
    // what was the output during that iteration?
    val regB = (regA % 8) ^ 2 // program: 2,4,1,2
    val regC = regA / math.pow(2, regB.toDouble).toLong // program: 7,5
    ((regB ^ 3) ^ regC) % 8 // program: 1,3,4,4,5,5

  def reverseEngineer(program: IndexedSeq[Int]): Long =
    def recurse(out: List[Int], regA: Long): Option[Long] =
      out match
        case Nil => Some(regA)
        case head :: tail =>
          val possibles = Iterator
            .range(regA * 8, regA * 8 + 8)
            .filter(loopOutput(_) == head)
          possibles.flatMap(recurse(tail, _)).minOption
    recurse(program.toList.reverse, 0L).get

  def part2(input: Input) = reverseEngineer(input.program)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
