package aoc2019

import scala.annotation.tailrec

object IntcodeComputer:

  type Program = IndexedSeq[Long]

  // UTIL

  def parseMemory(s: String): Program = s.split(",").map(_.toLong).toIndexedSeq

  def asInt(data: Long): Int = {
    if (Int.MinValue <= data && data <= Int.MaxValue) data.toInt
    else throw new Exception("Data not interpretable as Int.")
  }

  // OPCODE

  case class OpCode(id: Int, mode: Int)

  def parseOpCode(x: Long): OpCode = OpCode((x % 100).toInt, (x / 100).toInt)

  // Determine mode for a specific parameter.
  // Example: opcode 1002
  // Least two digits are the operation ID: "02"
  // Remaining digits define the mode for all parameters, least-to-most significant:
  // "10" => first parameter in mode 0, second parameter in mode 1
  // If unspecified: mode is 0
  def parameterMode(offset: Int, mode: Int): Int = {
    val shift = scala.math.pow(10, offset - 1).toInt
    (mode / shift) % 10
  }

  // STATE

  case class State(
      memory: Program,
      input: Iterator[Long] = Iterator.empty,
      output: Seq[Long] = Seq.empty,
      pointer: Int = 0,
      relBase: Int = 0
  ):
    def current    = memory(pointer)
    def isComplete = current == 99
    def needsInput = current == 3 && input.isEmpty

    // Supports reading past the end of current memory space (returning 0).
    private[this] def access(address: Int): Long = {
      if (address >= memory.size) 0L
      else memory(address)
    }

    def readData(offset: Int, mode: Int): Long = {
      val data = access(pointer + offset)
      parameterMode(offset, mode) match {
        // Positional Mode: read from the address defined in data
        case 0 => access(asInt(data))
        // Immediate Mode: read the data as a literal value
        case 1 => data
        // Relative Mode: like positional but add the relative base
        case 2 => access(asInt(data) + relBase)
        // Unknown
        case x => throw new Exception(s"Unsupported parameter mode $x")
      }
    }

    def readRegister(offset: Int, mode: Int): Int = {
      val data = asInt(access(pointer + offset))
      parameterMode(offset, mode) match {
        // Positional Mode: register is defined by data
        case 0 => data
        // Relative Mode: register is defined by data plus relative base
        case 2 => data + relBase
        // Unknown
        case x => throw new Exception(s"Unsupported parameter mode $x")
      }
    }

    def write(index: Int, value: Long, pointerAdvance: Int): State = {
      this.copy(
        memory = memory.padTo(index + 1, 0L).updated(index, value),
        pointer = pointer + pointerAdvance
      )
    }
  end State

  // OPERATIONS

  object Op {
    def inputOp(mode: Int, state: State): State = {
      val iR    = state.readRegister(1, mode)
      val value = state.input.next()
      state.write(iR, value, 2)
    }

    def outputOp(mode: Int, state: State): State = {
      val value = state.readData(1, mode)
      state.copy(
        output = state.output :+ value,
        pointer = state.pointer + 2
      )
    }

    def adjustRelBase(mode: Int, state: State): State = {
      val value = state.readData(1, mode)
      state.copy(
        relBase = state.relBase + asInt(value),
        pointer = state.pointer + 2
      )
    }

    def jumpOp(predicate: (Long) => Boolean)(mode: Int, state: State): State = {
      val a           = state.readData(1, mode)
      val j           = state.readData(2, mode)
      val nextPointer = if (predicate(a)) asInt(j) else state.pointer + 3
      state.copy(pointer = nextPointer)
    }
    val jumpIfTrue  = jumpOp(_ != 0) _
    val jumpIfFalse = jumpOp(_ == 0) _

    def testOp(comparison: (Long, Long) => Boolean)(mode: Int, state: State): State = {
      val a     = state.readData(1, mode)
      val b     = state.readData(2, mode)
      val iR    = state.readRegister(3, mode)
      val value = if (comparison(a, b)) 1 else 0
      state.write(iR, value, 4)
    }
    val lessThan = testOp(_ < _) _
    val equals   = testOp(_ == _) _

    def mathOp(operator: (Long, Long) => Long)(mode: Int, state: State): State = {
      val a     = state.readData(1, mode)
      val b     = state.readData(2, mode)
      val iR    = state.readRegister(3, mode)
      val value = operator(a, b)
      state.write(iR, value, 4)
    }
    val add      = mathOp(_ + _) _
    val multiply = mathOp(_ * _) _
  }

  // COMPUTATION

  final def step(state: State): State = {
    parseOpCode(state.current) match {
      // Normal op: process step.
      case OpCode(1, m) => Op.add(m, state)
      case OpCode(2, m) => Op.multiply(m, state)
      case OpCode(3, m) => Op.inputOp(m, state)
      case OpCode(4, m) => Op.outputOp(m, state)
      case OpCode(5, m) => Op.jumpIfTrue(m, state)
      case OpCode(6, m) => Op.jumpIfFalse(m, state)
      case OpCode(7, m) => Op.lessThan(m, state)
      case OpCode(8, m) => Op.equals(m, state)
      case OpCode(9, m) => Op.adjustRelBase(m, state)
      // Termination: we are done stepping.
      case OpCode(99, _) => state
      // Error: unknown opcode.
      case x => throw new Exception(s"Encountered unknown $x at index ${state.pointer}")
    }
  }

  /** Run program until is reports complete (op 99). */
  @tailrec
  final def run(state: State): State =
    if state.isComplete then state
    else run(step(state))

  /** Run program until it is asking for an input it doesn't have, or has halted. */
  final def runIOSync1(state: State, inputs: List[Long]): State =
    @tailrec
    def recurse(state: State): State =
      if state.isComplete || state.needsInput then state
      else recurse(step(state))

    recurse(state.copy(input = inputs.iterator, output = Seq.empty))

  final def runIO2(state: State, input: Iterator[Long]): State = {
    @tailrec
    def recurse(s: State): State = {
      if (s.output.nonEmpty) s
      else recurse(step(s))
    }
    val s0 = state.copy(input = input, output = Seq.empty)
    recurse(s0)
  }

end IntcodeComputer
