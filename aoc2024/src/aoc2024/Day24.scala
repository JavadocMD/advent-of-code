package aoc2024

import scala.annotation.tailrec
import aoc.Day
import scala.util.Try

object Day24 extends Day:

  case class Wire(name: String, value: Boolean)

  extension (ws: Set[Wire])
    def xs: Set[Wire] = ws.filter(_.name.startsWith("x"))
    def ys: Set[Wire] = ws.filter(_.name.startsWith("y"))
    def toLong: Long =
      ws.toList
        .sortBy(_.name)(Ordering[String].reverse)
        .foldLeft(0L): (acc, curr) =>
          (acc << 1) | (if curr.value then 1 else 0)

  extension (s: String)
    def isInput: Boolean  = s.startsWith("x") || s.startsWith("y")
    def isOutput: Boolean = s.startsWith("z")
    def bitIndex: Int     = s.tail.toInt
    def padLeft(n: Int, char: Char) =
      if n > s.size then (char.toString * (n - s.size)) + s else s
    def sprintln: Unit = println(s)

  sealed trait Gate:
    val in1: String
    val in2: String
    val out: String
    def compute(val1: Long, val2: Long): Long
    def withOutput(out: String): Gate

    def hasInput(a: String): Boolean             = in1 == a || in2 == a
    def hasInputs(a: String, b: String): Boolean = (in1 == a && in2 == b) || (in1 == b && in2 == a)

  case class AndGate(in1: String, in2: String, out: String) extends Gate:
    def compute(val1: Long, val2: Long): Long = val1 & val2
    def withOutput(out: String): AndGate      = copy(out = out)

  case class OrGate(in1: String, in2: String, out: String) extends Gate:
    def compute(val1: Long, val2: Long): Long = val1 | val2
    def withOutput(out: String): OrGate       = copy(out = out)

  case class XorGate(in1: String, in2: String, out: String) extends Gate:
    def compute(val1: Long, val2: Long): Long = val1 ^ val2
    def withOutput(out: String): XorGate      = copy(out = out)

  type Input = (Set[Wire], Set[Gate])

  def parse(xs: List[String]): Input =
    val (wires, rest) = xs.span(_.nonEmpty)
    val gates         = rest.drop(1)

    def sortInputs(in1: String, in2: String): (String, String) =
      if in1 <= in2 then (in1, in2) else (in2, in1)

    val w = wires.map:
      case s"$name: $value" => Wire(name, value.toInt == 1)
    val g = gates.map:
      case s"$in1 AND $in2 -> $out" =>
        val (i1, i2) = sortInputs(in1, in2)
        AndGate(i1, i2, out)
      case s"$in1 OR $in2 -> $out" =>
        val (i1, i2) = sortInputs(in1, in2)
        OrGate(i1, i2, out)
      case s"$in1 XOR $in2 -> $out" =>
        val (i1, i2) = sortInputs(in1, in2)
        XorGate(i1, i2, out)
    (w.toSet, g.toSet)

  def toWireMap(xy: "x" | "y", n: Long): Map[String, Long] =
    Map.from(
      for
        i <- Iterator.range(0, 45)
        s = f"$xy$i%02d"
      yield s -> ((n >> i) & 1)
    )

  def computer(gates: Set[Gate], wires: Set[Wire], swaps: List[(String, String)] = Nil): (Long, Long) => Long =
    @tailrec
    def sortGates(xs: Set[Gate], known: Set[String], acc: List[Gate]): List[Gate] =
      if xs.isEmpty then acc.reverse
      else
        xs.find(g => known(g.in1) && known(g.in2)) match
          case None    => throw Exception("invalid computer")
          case Some(g) => sortGates(xs - g, known + g.out, g :: acc)

    val swappedGates = swaps.foldLeft(gates):
      case (gs, (a, b)) =>
        val ga = gs.find(_.out == a).get
        val gb = gs.find(_.out == b).get
        gs - ga - gb + ga.withOutput(b) + gb.withOutput(a)
    val sortedGates = sortGates(swappedGates, wires.map(_.name), Nil)

    def solve(x: Long, y: Long): Long =
      @tailrec
      def recurse(xs: List[Gate], known: Map[String, Long], acc: Long = 0L): Long =
        xs match
          case Nil => acc
          case head :: tail =>
            val result = head.compute(known(head.in1), known(head.in2))
            val output = if !head.out.isOutput then 0L else (result << head.out.bitIndex)
            recurse(tail, known + (head.out -> result), acc | output)

      val initial = toWireMap("x", x) ++ toWireMap("y", y)
      recurse(sortedGates, initial)
    solve

  def part1(input: Input) =
    val (ws, gs) = input
    computer(gs, ws)(ws.xs.toLong, ws.ys.toLong)

  // NOTES:
  // - x and y are 45 bits
  // - z is 46 bits
  // - only input wires start with x or y and only outputs start with z

  def part2(input: Input) =
    // (This is more of a tool-assisted manual solution than a fully automated one.)
    // This solution works by attempting to find the expected full-adder structure
    // for each bit in sequence.
    // If it fails to find the structure, it goes into debugging mode and prints out all the gates
    // it was able to find.
    // The operator (me) then visually inspects the gates to figure out which gate to swap,
    // and alters the program to include that swap before running it again.
    // Repeat until all four swaps are found.

    val (wires, gates) = input

    def nextGates(gate: Gate): Set[Gate] = gates.filter: g =>
      g.in1 == gate.out || g.in2 == gate.out

    // --- MANUALLY ENTER DISCOVERED SWAPS HERE --------------------------------------
    val swaps = Seq(("nbc", "svm"), ("kqk", "z15"), ("z23", "cgq"), ("z39", "fnr"))
    // --- END MANUALLY ENTERED SWAPS AND MAY SANTA HAVE MERCY UPON ME ---------------

    var gatesRemaining = swaps.foldLeft(gates):
      case (gs, (a, b)) =>
        val ga = gs.find(_.out == a).get
        val gb = gs.find(_.out == b).get
        gs - ga - gb + ga.withOutput(b) + gb.withOutput(a)

    var carryIn = Map.empty[Int, String]

    // Deal with first bit: special structure
    val (x0, y0, z0) = ("x00", "y00", "z00")
    val ha1xorb0 = gatesRemaining
      .find: g =>
        g.isInstanceOf[XorGate] && g.in1 == x0 && g.in2 == y0
      .get // we can always find the HA1XOR
    val ha1andb0 = gatesRemaining
      .find: g =>
        g.isInstanceOf[AndGate] && g.in1 == x0 && g.in2 == y0
      .get // we can always find the HA1AND
    carryIn += (1 -> ha1andb0.out)
    gatesRemaining --= Seq(ha1xorb0, ha1andb0)
    println("bit 0 good")

    for
      bit <- 1 until 45
      if carryIn.contains(bit)
    do
      val (x, y, z) = (f"x$bit%02d", f"y$bit%02d", f"z$bit%02d")
      val cin       = carryIn(bit)

      val ha1xor = gatesRemaining
        .find: g =>
          g.isInstanceOf[XorGate] && g.in1 == x && g.in2 == y
        .get // we can always find the HA1XOR; input wires can't be swapped
      val ha1and = gatesRemaining
        .find: g =>
          g.isInstanceOf[AndGate] && g.in1 == x && g.in2 == y
        .get // we can always find the HA1AND; input wires can't be swapped

      val success = for
        ha2xor <- gatesRemaining.find: g =>
          g.isInstanceOf[XorGate] && g.hasInputs(cin, ha1xor.out) && g.out == z
        ha2and <- gatesRemaining.find: g =>
          g.isInstanceOf[AndGate] && g.hasInputs(cin, ha1xor.out)
        cout <- gatesRemaining.find: g =>
          g.isInstanceOf[OrGate] && g.hasInputs(ha1and.out, ha2and.out)
      yield (ha2xor, ha2and, cout)

      if success.isEmpty then
        println(s"== DEBUGGIN BIT $bit")
        println(s"cin: $cin")
        println(s"ha1xor: $ha1xor -> ${nextGates(ha1xor).mkString(", ")}")
        println(s"ha1and: $ha1and -> ${nextGates(ha1and).mkString(", ")}")
        println(s"BIT $bit BORK")

      success match
        case None => // noop
        case Some((ha2xor, ha2and, cout)) =>
          println(s"bit $bit good")
          gatesRemaining --= Seq(ha1xor, ha1and, ha2xor, ha2and, cout)
          carryIn += (bit + 1 -> cout.out)

    // Didn't bother checking output bit 45 because I'd already found the four swaps.

    swaps.map((a, b) => Seq(a, b)).flatten.sorted.mkString(",")

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
