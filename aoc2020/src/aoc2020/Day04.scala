package aoc2020

import scala.io.Source
import scala.annotation.tailrec
import scala.util.matching.Regex

import aoc.Day

object Day04 extends Day:
  type Input = Array[String]
  def parse(xs: Array[String]): Input = xs

  val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val eyeColors      = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  def isNumber(min: Int, max: Int)(s: String): Boolean = {
    s.toIntOption.filter(n => min <= n && n <= max).isDefined
  }
  def isRegex(r: Regex)(s: String): Boolean = r.matches(s)

  val rules: Map[String, String => Boolean] = Map(
    "byr" -> isNumber(1920, 2002) _,
    "iyr" -> isNumber(2010, 2020) _,
    "eyr" -> isNumber(2020, 2030) _,
    "hgt" -> (s => {
      (s.dropRight(2), s.takeRight(2)) match {
        case (n, "cm") => isNumber(150, 193)(n)
        case (n, "in") => isNumber(59, 76)(n)
        case _         => false
      }
    }),
    "hcl" -> isRegex("^#[0-9a-f]{6}$".r) _,
    "ecl" -> eyeColors.contains _,
    "pid" -> isRegex("^[0-9]{9}$".r) _,
    "cid" -> (s => true)
  )

  val batches = batchIterator[String](identity, _ + " " + _) _

  // Checks that all required fields are present.
  def isValid1(passport: String): Boolean = {
    var fields = requiredFields
    passport.split(" ").foreach { s =>
      val Array(name, _) = s.split(':')
      fields -= name
    }
    fields.isEmpty
  }

  def part1(input: Input): Int = batches(input).count(isValid1)

  // Checks required fields are present and values pass field validation.
  def isValid2(passport: String): Boolean = {
    var fields = requiredFields
    passport.split(" ").foreach { s =>
      val Array(name, value) = s.split(':')
      val valid              = rules(name)(value)
      if (valid) {
        fields -= name
      }
    }
    fields.isEmpty
  }

  def part2(input: Input): Int = batches(input).count(isValid2)

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput(preserveBlanks = true))
    solveP1(() => part1(in))
    solveP2(() => part2(in))
