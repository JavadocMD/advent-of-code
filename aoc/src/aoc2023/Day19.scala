package aoc2023

import scala.annotation.tailrec
import aoc.Day

object Day19 extends Day:

  case class Part(x: Int, m: Int, a: Int, s: Int)

  sealed trait ClausResult
  case class Next(name: String) extends ClausResult
  case object Reject            extends ClausResult
  case object Accept            extends ClausResult

  type Xmas = 'x' | 'm' | 'a' | 's'
  type Op   = '<' | '>'

  sealed trait Claus
  case class ImmediateClaus(result: ClausResult) extends Claus
  case class ConditionalClaus(key: Xmas, op: Op, value: Int, result: ClausResult) extends Claus:
    val predicate: Part => Boolean =
      val comparator: (Int, Int) => Boolean = op match
        case '>' => _ > _
        case '<' => _ < _
      key match
        case 'x' => part => comparator(part.x, value)
        case 'm' => part => comparator(part.m, value)
        case 'a' => part => comparator(part.a, value)
        case 's' => part => comparator(part.s, value)

  case class Rule(name: String, clauses: List[Claus])

  case class Input(rules: List[Rule], parts: List[Part])

  def parse(xs: List[String]): Input =
    def key(keyStr: String): Xmas = keyStr match
      case "x" => 'x'
      case "m" => 'm'
      case "a" => 'a'
      case "s" => 's'
      case _   => throw Exception(s"Unexpected key $keyStr")

    def result(resultStr: String): ClausResult = resultStr match
      case "R"  => Reject
      case "A"  => Accept
      case name => Next(name)

    val (rulesStr, partsStr) = xs.span(_.nonEmpty)
    val rules = rulesStr.map:
      case s"$name{$clauseStr}" =>
        val clauses = clauseStr
          .split(",")
          .toList
          .map:
            case s"$keyStr>$valueStr:$resultStr" =>
              ConditionalClaus(key(keyStr), '>', valueStr.toInt, result(resultStr))
            case s"$keyStr<$valueStr:$resultStr" =>
              ConditionalClaus(key(keyStr), '<', valueStr.toInt, result(resultStr))
            case "R"  => ImmediateClaus(Reject)
            case "A"  => ImmediateClaus(Accept)
            case name => ImmediateClaus(Next(name))
        Rule(name, clauses)

    val parts = partsStr.tail.map:
      case s"{x=$x,m=$m,a=$a,s=$s}" => Part(x.toInt, m.toInt, a.toInt, s.toInt)

    Input(rules, parts)

  def eval(part: Part, rules: Map[String, Rule]): ClausResult =
    @tailrec
    def recurse(clauses: List[Claus]): ClausResult =
      clauses match
        case Nil                             => throw Exception("ran out of clauses!")
        case ImmediateClaus(Accept) :: _     => Accept
        case ImmediateClaus(Reject) :: _     => Reject
        case ImmediateClaus(Next(name)) :: _ => recurse(rules(name).clauses)
        case (c @ ConditionalClaus(_, _, _, result)) :: tail =>
          if !c.predicate(part) then recurse(tail)
          else
            result match
              case Accept     => Accept
              case Reject     => Reject
              case Next(name) => recurse(rules(name).clauses)

    recurse(rules("in").clauses)

  def part1(input: Input) =
    val rules = input.rules.map(x => (x.name -> x)).toMap
    input.parts
      .filter(p => eval(p, rules) == Accept)
      .map(p => p.x + p.m + p.a + p.s)
      .sum

  case class RangePart(x: Range, m: Range, a: Range, s: Range):
    def span(key: Xmas, op: Op, value: Int): (RangePart, RangePart) =
      (key, op) match
        case ('x', '<') => (this.copy(x = Range(x.start, value)), this.copy(x = Range(value, x.end)))
        case ('x', '>') => (this.copy(x = Range(value + 1, x.end)), this.copy(x = Range(x.start, value + 1)))
        case ('m', '<') => (this.copy(m = Range(m.start, value)), this.copy(m = Range(value, m.end)))
        case ('m', '>') => (this.copy(m = Range(value + 1, m.end)), this.copy(m = Range(m.start, value + 1)))
        case ('a', '<') => (this.copy(a = Range(a.start, value)), this.copy(a = Range(value, a.end)))
        case ('a', '>') => (this.copy(a = Range(value + 1, a.end)), this.copy(a = Range(a.start, value + 1)))
        case ('s', '<') => (this.copy(s = Range(s.start, value)), this.copy(s = Range(value, s.end)))
        case ('s', '>') => (this.copy(s = Range(value + 1, s.end)), this.copy(s = Range(s.start, value + 1)))

    def isEmpty: Boolean = x.isEmpty || m.isEmpty || a.isEmpty || s.isEmpty

  val start = RangePart(Range(1, 4001), Range(1, 4001), Range(1, 4001), Range(1, 4001))

  def rangeEval(rules: Map[String, Rule]): List[RangePart] =

    def recurse(range: RangePart, clauses: List[Claus]): List[RangePart] =
      clauses match
        case Nil                             => throw Exception("ran out of clauses!")
        case ImmediateClaus(Accept) :: _     => List(range)
        case ImmediateClaus(Reject) :: _     => Nil
        case ImmediateClaus(Next(name)) :: _ => recurse(range, rules(name).clauses)
        case ConditionalClaus(key, op, value, result) :: tail =>
          val (success, fail) = range.span(key, op, value)

          val lefts =
            if (success.isEmpty) then Nil
            else
              result match
                case Accept     => List(success)
                case Reject     => Nil
                case Next(name) => recurse(success, rules(name).clauses)

          val rights =
            if (fail.isEmpty) then Nil
            else recurse(fail, tail)

          lefts ::: rights

    recurse(start, rules("in").clauses)

  def part2(input: Input) =
    val rules = input.rules.map(x => (x.name -> x)).toMap
    rangeEval(rules)
      .map: r =>
        r.x.size.toLong * r.m.size * r.a.size * r.s.size
      .sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
