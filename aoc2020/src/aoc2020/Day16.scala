package aoc2020

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.{mutable => m}

import aoc.Day

object Day16 extends Day:

  def parse(xs: Array[String]): Input = Input.from(xs)

  case class Range(min: Int, max: Int)

  case class Field(name: String, a: Range, b: Range):
    val valid = {
      val arr = Array.fill[Boolean](1000)(false)
      for { n <- a.min to a.max } { arr(n) = true }
      for { n <- b.min to b.max } { arr(n) = true }
      arr
    }

  object Field:
    import fastparse._
    import fastparse.NoWhitespace._
    import fastparse.Parsed.{Success, Failure}

    def word[$: P]  = P(CharIn("a-z").rep)
    def num[$: P]   = P(CharIn("0-9").rep).!.map(_.toInt)
    def range[$: P] = P(num ~ "-" ~ num).map({ case (min, max) => Range(min, max) })
    def field[$: P] =
      P(Start ~ word.rep(sep = " ").! ~ ": " ~ range ~ " or " ~ range ~ End)
        .map { case (n, a, b) => Field(n, a, b) }

    def from(s: String): Field = fastparse.parse(s, field(_)) match
      case Success(r, _) => r
      case _             => throw new Exception(s"Unable to parse $s")

  type Ticket = Array[Int]

  case class Input(fields: List[Field], yours: Ticket, nearby: List[Ticket]):
    // for every n from 0-1000, is n in range for at least one rule?
    val valid = {
      val arr = Array.fill[Boolean](1000)(false)
      fields.foreach(r => {
        for { n <- r.a.min to r.a.max } { arr(n) = true }
        for { n <- r.b.min to r.b.max } { arr(n) = true }
      })
      arr
    }

  object Input:
    def from(input: Array[String]): Input = {
      val chunks = toChunks(input)
      val fields = chunks(0).map(Field.from).toList
      val yours  = chunks(1).drop(1).head.split(",").map(_.toInt)
      val nearby = chunks(2).drop(1).map(_.split(",").map(_.toInt)).toList
      Input(fields, yours, nearby)
    }

  def part1(input: Input): Int =
    // find all invalid numbers and add them
    val invalidNumbers = for {
      t <- input.nearby
      n <- t if !input.valid(n)
    } yield n
    invalidNumbers.sum

  def identifyColumns(fields: List[Field], tickets: List[Ticket]): Map[Field, Int] =
    // working set:
    //   find all possible columns for each field
    def validColumns(f: Field): Set[Int] =
      // for all tickets, consider all columns...
      var cs = Set.range(0, fields.size)
      for { t <- tickets; i <- 0 until t.size } {
        // if a column proves invalid for this field,
        // remove the column from the valid set
        if (!f.valid(t(i))) {
          cs -= i
        }
      }
      cs

    val working: m.Map[Field, Set[Int]] = m.Map.from(
      fields.map(f => f -> validColumns(f))
    )

    // result set:
    //   find a field with only one option, add it to results, weed out that column, and repeat
    val result: m.Map[Field, Int] = m.Map.empty
    while (working.nonEmpty) {
      val (field, colSet) = working.find({ case (f, cs) => cs.size == 1 }).get
      val col             = colSet.head
      result(field) = col
      working -= field
      working.foreach { case (f, cs) =>
        working(f) = cs - col
      }
    }
    result.toMap
  end identifyColumns

  def part2(input: Input): Long =
    val Input(fields, yours, nearby) = input
    def isValid(t: Ticket)           = !t.exists(n => !input.valid(n))
    val tickets                      = nearby.filter(isValid)
    val fieldColumns                 = identifyColumns(fields, tickets)
    val criticalValues = for {
      f <- fields if f.name.startsWith("departure")
      i = fieldColumns(f)
    } yield yours(i).toLong
    criticalValues.product

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
