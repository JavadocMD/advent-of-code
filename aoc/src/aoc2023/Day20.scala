package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day20 extends Day:

  enum Pulse:
    case High, Low
  import Pulse._

  object Pulse:
    def or(a: Pulse, b: Pulse): Pulse =
      if a == High || b == High then High else Low

    def and(a: Pulse, b: Pulse): Pulse =
      if a == High && b == High then High else Low

    def not(a: Pulse): Pulse =
      if a == High then Low else High

  case class Send(from: String, to: String, value: Pulse)

  sealed trait Module:
    def name: String
    def out: Vector[String]

  case class Broadcaster(out: Vector[String]) extends Module:
    val name = "broadcaster"

  case class FlipFlop(name: String, out: Vector[String]) extends Module

  case class Conjunction(name: String, out: Vector[String]) extends Module

  case class Debug(name: String) extends Module:
    val out = Vector.empty

  type Input = List[Module]

  def parse(xs: List[String]): Input =
    xs.map:
      case s"broadcaster -> $outputs" => Broadcaster(outputs.split(", ").toVector)
      case s"%$name -> $outputs"      => FlipFlop(name, outputs.split(", ").toVector)
      case s"&$name -> $outputs"      => Conjunction(name, outputs.split(", ").toVector)

  class ModuleInfo(val modules: List[Module]):
    val moduleMap = modules.map(m => m.name -> m).toMap

    def inputsFor(name: String): List[String] =
      modules.filter(_.out.contains(name)).map(_.name)

  class Simulator(info: ModuleInfo):
    import scala.collection.{mutable as m}

    // State storage
    case class FlipFlopState(value: Pulse)
    case class ConjunctionState(inputs: Map[String, Pulse])

    var flipFlops = m.HashMap.from(
      info.modules
        .collect({ case x: FlipFlop => x })
        .map: m =>
          m.name -> FlipFlopState(Low),
    )

    var conjunctions = m.HashMap.from(
      info.modules
        .collect({ case x: Conjunction => x })
        .map: m =>
          val inputs = info.inputsFor(m.name).map(_ -> Low).toMap
          val state  = ConjunctionState(inputs)
          m.name -> state,
    )

    var presses = 0L

    // Get the target module to handle the given input signal.
    // Update state and return any Pulse that should be sent to its outputs.
    private def handle(target: Module, signal: Send): Option[Pulse] = target match
      case m: Broadcaster => Some(signal.value)
      case m: Debug       => None
      case m: FlipFlop =>
        if signal.value == High then None
        else
          val state     = flipFlops(m.name)
          val nextValue = Pulse.not(state.value)
          flipFlops(m.name) = FlipFlopState(nextValue)
          Some(nextValue)
      case m: Conjunction =>
        val state     = conjunctions(m.name)
        val nextState = ConjunctionState(state.inputs.updated(signal.from, signal.value))
        conjunctions(m.name) = nextState
        val sending = if nextState.inputs.values.forall(_ == High) then Low else High
        Some(sending)

    def press: List[Send] =
      presses += 1
      var sent  = List.empty[Send]
      val queue = m.Queue(Send("button", "broadcaster", Low))
      while queue.nonEmpty do
        val signal = queue.dequeue()
        sent ::= signal
        val target = info.moduleMap(signal.to)
        handle(target, signal) match
          case None => // nothing to do
          case Some(pulse) =>
            val sending = target.out.map(Send(target.name, _, pulse))
            queue.enqueueAll(sending)
      sent.reverse

    def pressUntilHigh(watched: String): Long =
      val queue    = m.Queue.empty[Send]
      var detected = false
      while !detected do
        queue += Send("button", "broadcaster", Low)
        presses += 1
        while queue.nonEmpty do
          val signal = queue.dequeue()
          val target = info.moduleMap(signal.to)
          handle(target, signal) match
            case None => // nothing to do
            case Some(pulse) =>
              if target.name == watched && pulse == High then detected = true
              val sending = target.out.map(Send(target.name, _, pulse))
              queue.enqueueAll(sending)
      presses

  def part1(input: Input) =
    val info = ModuleInfo(input :+ Debug("rx"))
    val sim  = Simulator(info)

    val (low, high) = (0 until 1000).foldLeft((0L, 0L)):
      case ((prevLow, prevHigh), _) =>
        val sent     = sim.press
        val currHigh = sent.count(_.value == High)
        val currLow  = sent.size - currHigh
        (prevLow + currLow, prevHigh + currHigh)

    low * high

  def part2(input: Input) =

    // The input to "rx" is "mf", a conjunction
    // The inputs to "mf" are "bh", "jf", "sh", and "mz"
    // It turns out those modules are almost always low, but go high once
    // in a relatively short period. In that case, we just need to find
    // the press on which all four go high, which is the LCM of their individual periods.

    // Inputs to the inputs of rx
    val info = ModuleInfo(input :+ Debug("rx"))
    val criticalModules = for
      m1 <- info.inputsFor("rx")
      m2 <- info.inputsFor(m1)
    yield m2

    // We'll do a bit extra to verify periodic behavior, but really they were engineered to be simple.
    val periods = criticalModules.map: name =>
      val sim     = Simulator(info)
      val presses = (0 until 4).map(_ => sim.pressUntilHigh(name)).toList
      val diffs = presses
        .zip(presses.tail)
        .map({ case (a, b) => b - a })
      val allEqual = diffs.forall(_ == diffs.head)
      if !allEqual then throw Exception(s"Unable to detect periodic behavior in module '$name'")
      diffs.head

    periods.reduce(lcm(_, _))

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
