package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Util._

object Day10 extends Day:

  sealed trait Entity
  case class Bot(id: Int)    extends Entity
  case class Output(id: Int) extends Entity

  case class Comparison(bot: Bot, low: Entity, high: Entity)

  case class Input(initialChips: List[(Int, Bot)], cmps: Map[Bot, Comparison])

  lazy val input =
    val lines = loadInput()
    val initial = lines.collect:
      case s"value $value goes to bot $bot" => (value.toInt, Bot(bot.toInt))
    val cmps = lines
      .collect:
        case s"bot $bot gives low to $low and high to $high" =>
          val elow = low match
            case s"bot $bot"    => Bot(bot.toInt)
            case s"output $out" => Output(out.toInt)
          val ehigh = high match
            case s"bot $bot"    => Bot(bot.toInt)
            case s"output $out" => Output(out.toInt)
          Comparison(Bot(bot.toInt), elow, ehigh)
      .map(c => c.bot -> c)
      .toMap
    Input(initial, cmps)

  extension [E](entities: Map[E, List[Int]]) //
    def drop2(e: E): Map[E, List[Int]]           = entities.updated(e, entities(e).dropRight(2))
    def take(e: E, chip: Int): Map[E, List[Int]] = entities.updated(e, chip :: entities(e))

  case class State(bots: Map[Bot, List[Int]], outputs: Map[Output, List[Int]]):
    def resolveBots(cmp: Comparison, low: Int, high: Int): Map[Bot, List[Int]] =
      cmp match
        case Comparison(curr, Bot(lowId), Bot(highId)) =>
          bots.drop2(curr).take(Bot(lowId), low).take(Bot(highId), high)
        case Comparison(curr, Bot(lowId), _) =>
          bots.drop2(curr).take(Bot(lowId), low)
        case Comparison(curr, _, Bot(highId)) =>
          bots.drop2(curr).take(Bot(highId), high)
        case Comparison(curr, _, _) =>
          bots.drop2(curr)

    def resolveOutputs(cmp: Comparison, low: Int, high: Int): Map[Output, List[Int]] =
      cmp match
        case Comparison(curr, Output(lowId), Output(highId)) =>
          outputs.take(Output(lowId), low).take(Output(highId), high)
        case Comparison(curr, Output(lowId), _) =>
          outputs.take(Output(lowId), low)
        case Comparison(curr, _, Output(highId)) =>
          outputs.take(Output(highId), high)
        case Comparison(curr, _, _) =>
          outputs

    def step(cmps: Map[Bot, Comparison]): Option[(State, (Bot, Int, Int))] =
      bots
        .find:
          case (_, chips) => chips.size >= 2
        .map:
          case (curr, _) =>
            val cmp                = cmps(curr)
            val low :: high :: Nil = bots(curr).takeRight(2).sorted: @unchecked
            val nextBots           = resolveBots(cmp, low, high)
            val nextOuts           = resolveOutputs(cmp, low, high)
            (State(nextBots, nextOuts), (curr, low, high))

  object State:
    def initial(chips: List[(Int, Bot)]): State =
      val bots = chips.foldLeft(Map.empty[Bot, List[Int]].withDefaultValue(Nil)):
        case (acc, (chip, bot)) => acc.updated(bot, chip :: acc(bot))
      val outs = Map.empty[Output, List[Int]].withDefaultValue(Nil)
      State(bots, outs)

  lazy val part1 =
    val z = (State.initial(input.initialChips), (Bot(-1), -1, -1))
    Iterator
      .unfold(z):
        case (prev, _) => prev.step(input.cmps).map(x => (x, x))
      .find:
        case (_, (bot, low, high)) => low == 17 && high == 61
      .map:
        case (_, (bot, _, _)) => bot.id
      .get

  lazy val part2 =
    val result = Iterator
      .unfold(State.initial(input.initialChips)):
        case prev => prev.step(input.cmps).map(x => (x._1, x._1))
      .lastOption
      .get
    val outs = result.outputs
    outs(Output(0)).head * outs(Output(1)).head * outs(Output(2)).head

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
