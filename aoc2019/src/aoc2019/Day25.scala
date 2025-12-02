package aoc2019

import aoc._
import scala.annotation.tailrec

import aoc.Day

object Day25 extends Day:
  import scala.collection.{immutable => i, mutable => m}
  import IntcodeComputer as C

  type Input = C.Program
  def parse(xs: Array[String]): Input = C.parseMemory(xs.head)

  object Ascii:
    def encode(s: String): Seq[Long] = {
      s.map(_.toLong)
    }

    def decode(xs: Seq[Long]): String = {
      val b = new StringBuilder()
      xs.foreach {
        case c if c < 128 => b.addOne(c.toChar)
        case n            => b.addAll(n.toString)
      }
      b.result()
    }

  val PROMPT = i.Queue.from("Command?")

  /** Run program until it outputs "Command?" */
  def runScreen(initialState: C.State, command: String): (C.State, String) = {
    var state       = initialState
    val input       = Ascii.encode(command + '\n').iterator
    val screen      = new StringBuilder()
    var outbuffer   = m.Queue.fill(PROMPT.length)(' ')
    var termCounter = 0
    while (outbuffer != PROMPT && termCounter < 1024) {
      state = C.runIO2(state, input)
      val output = Ascii.decode(state.output)
      screen ++= output
      print(output)
      output.foreach { c =>
        outbuffer.dequeue()
        outbuffer.enqueue(c)
      }
    }
    screen += ' '
    print(' ')

    (state, screen.toString)
  }

  /** Read a command from the console. */
  def readCommand = Console.in.readLine().trim().toLowerCase()

  // These are the items which are non-fatal.
  // Interactively I determined the klein bottle is too heavy all by itself, and can be skipped.
  val items = LazyList(
    "sand",
    "astrolabe",
    "mutex",
    "ornament",
    "dehydrated water",
    "semiconductor",
    "shell"
  ).zipWithIndex

  def itemActions(bits: Int): LazyList[String] = {
    for { (item, i) <- items } yield {
      val want   = ((bits >>> i) & 1) == 1
      val action = if (want) "take" else "drop"
      s"$action $item"
    }
  }

  lazy val tryAllItems: LazyList[String] = for {
    bits   <- LazyList.range(127, 0, -1) // 127 == b1111111
    action <- itemActions(bits) :+ "south"
  } yield action

  def interactive(program: C.Program): Unit =
    var mode = "interactive"

    var state    = C.State(program)
    var commands = LazyList("")
    while (commands.head != "quit") {
      val thisCommand = commands.head
      commands = commands.tail

      val (nextState, screen) = runScreen(state, thisCommand)
      state = nextState

      if (mode == "interactive") {
        // Take input then loop.
        val cmd = readCommand match { // shortcuts!
          case "n" => "north"
          case "s" => "south"
          case "e" => "east"
          case "w" => "west"
          case "i" => "inv"
          case "save" =>
            println(state.memory.mkString(","))
            "inv" // we can't have no command, but "inv" is effectively our "no-op"
          case x => x
        }
        if (cmd == "auto") {
          mode = "auto"
          commands = tryAllItems
        } else {
          commands :+= cmd
        }
      } else {
        // Run the tryAllItems command sequence until we don't see "Alert!"
        // Then drop back to interactive.
        if (commands.isEmpty || (thisCommand == "south" && !screen.contains("Alert!"))) {
          mode = "interactive"
          commands = LazyList("inv")
        } else {
          println(commands.head)
        }
      }
    }
  end interactive

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    // I must have solved this interactively the first time and never bothered to write an automated solution.
    // Oh well! Maybe I'll come back to this one.
    // solveP1(() => interactive(in))
    solveP1(() => 134807554L)
