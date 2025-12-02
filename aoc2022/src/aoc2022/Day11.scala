package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day11 extends Day:
  type Item = Long
  case class Monkey(op: Item => Item, testMod: Int, throwTrue: Int, throwFalse: Int)

  type Input = (Vector[Monkey], Vector[Vector[Item]]) // (monkeys, starting items)
  def parse(xs: Iterator[String]): Input =
    xs.grouped(7)
      .toVector
      .map { lines =>
        val items = lines(1) match {
          case s"  Starting items: $ns" => ns.split(", ").map(_.toLong).toVector
        }
        val op = lines(2) match {
          case s"  Operation: new = old * old"        => (x: Item) => x * x
          case s"  Operation: new = old * ${IntP(n)}" => (x: Item) => x * n
          case s"  Operation: new = old + ${IntP(n)}" => (x: Item) => x + n
        }
        val testMod = lines(3) match {
          case s"  Test: divisible by ${IntP(n)}" => n
        }
        val throwTrue = lines(4) match {
          case s"    If true: throw to monkey ${IntP(n)}" => n
        }
        val throwFalse = lines(5) match {
          case s"    If false: throw to monkey ${IntP(n)}" => n
        }
        (Monkey(op, testMod, throwTrue, throwFalse), items)
      }
      .unzip

  case class State(items: Vector[Vector[Item]], inspections: Vector[Long])

  def simulate(monkeys: Vector[Monkey]): State => State =
    val overallModulus = monkeys.map(_.testMod).product
    initialState =>
      var State(its, ins) = initialState
      for (m, i) <- monkeys.zipWithIndex do
        ins = ins.updated(i, ins(i) + its(i).size)
        for item <- its(i) do
          val itemNow = m.op(item) % overallModulus
          val dest    = if itemNow % m.testMod == 0 then m.throwTrue else m.throwFalse
          its = its.updated(dest, its(dest) :+ itemNow)
        its = its.updated(i, Vector.empty)
      State(its, ins)

  def part1(input: Input) =
    val (monkeys, items)      = input
    val starting              = State(items, Vector.fill(monkeys.size)(0L))
    val sim                   = simulate(monkeys.map { m => m.copy(op = (x) => m.op(x) / 3) })
    val State(_, inspections) = (0 until 20).foldLeft(starting) { (state, _) => sim(state) }
    inspections.sorted(Ordering[Long].reverse).take(2).product

  def part2(input: Input) =
    val (monkeys, items)      = input
    val starting              = State(items, Vector.fill(monkeys.size)(0L))
    val sim                   = simulate(monkeys)
    val State(_, inspections) = (0 until 10000).foldLeft(starting) { (state, _) => sim(state) }
    inspections.sorted(Ordering[Long].reverse).take(2).product

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))

  object IntP:
    def unapply(s: String): Option[Int] =
      try Some(s.toInt)
      catch { case _: NumberFormatException => None }
