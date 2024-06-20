package aoc2020

import scala.io.Source
import scala.annotation.tailrec

import aoc.Day

object Day19 extends Day:
  type Id = String

  sealed trait Rule
  case class Sequence(rules: Id*)                extends Rule
  case class Or(left: Sequence, right: Sequence) extends Rule
  case object A                                  extends Rule
  case object B                                  extends Rule

  def parseRules(rules: Array[String]): Map[Id, Rule] =
    def parseText(s: String): Rule = {
      if (s == "\"a\"") A
      else if (s == "\"b\"") B
      else if (!s.contains('|')) Sequence(s.split(' '): _*)
      else {
        val Array(a, b) = s.split('|')
        Or(
          Sequence(a.trim.split(' '): _*),
          Sequence(b.trim.split(' '): _*)
        )
      }
    }

    Map.from(for {
      s <- rules
      Array(id, txt) = s.split(':')
    } yield id -> parseText(txt.trim))
  end parseRules

  def check(string: String, ruleMap: Map[Id, Rule]): Boolean = {
    // At each stage there is a list of char-lists to consider (branches)
    // based on which rules have been applied so far. This is specifically
    // necessary for Or rules that might initially match both sides, but where
    // only one side will go on to yield a successful match.
    def recurse(branches: List[List[Char]], ruleId: Id): List[List[Char]] = {

      def checkSeq(xs: List[Char], s: Sequence): List[List[Char]] = {
        s.rules.foldLeft(List(xs)) {
          case (Nil, _) => Nil
          case (bs, r)  => recurse(bs, r)
        }
      }

      val rule = ruleMap(ruleId)
      branches.flatMap { xs =>
        (xs, rule) match {
          case (Nil, _)                         => Nil // 0 chars but 1+ rules!
          case (head :: tail, A) if head == 'a' => List(tail)
          case (head :: tail, B) if head == 'b' => List(tail)
          case (_, A)                           => Nil
          case (_, B)                           => Nil
          case (_, Or(a, b))                    => checkSeq(xs, a) ++ checkSeq(xs, b)
          case (_, s: Sequence)                 => checkSeq(xs, s)
        }
      }
    }

    // Successful matches will yield an empty list, and we must have at least one.
    val results = recurse(List(string.toList), "0")
    results.exists(_.isEmpty)
  }

  def part1(rules: Array[String], messages: Array[String]): Int = {
    val ruleMap = parseRules(rules)
    messages.count(check(_, ruleMap))
  }

  def part2(rules: Array[String], messages: Array[String]): Int = {
    val ruleMap = parseRules(rules)
      .updated("8", Or(Sequence("42"), Sequence("42", "8")))
      .updated("11", Or(Sequence("42", "31"), Sequence("42", "11", "31")))
    messages.count(check(_, ruleMap))
  }

  final def main(args: Array[String]): Unit =
    val in       = loadInput()
    val chunk    = toChunks(in)
    val rules    = chunk(0)
    val messages = chunk(1)
    solveP1(() => part1(rules, messages))
    solveP2(() => part2(rules, messages))
