package aoc2020

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.immutable.MultiDict

import aoc.Day

object Day21 extends Day:
  import scala.collection.{mutable => m}

  type Input = Array[Food]
  def parse(xs: Array[String]): Input = xs.map(Food.from)

  case class Food(ingredients: Set[String], allergies: Set[String])

  object Food:
    def from(s: String): Food =
      val s1           = s.replaceAll("[,()]", "").split(' ')
      val (ings, algs) = s1.splitAt(s1.indexOf("contains"))
      Food(ings.toSet, algs.toSet - "contains")

  def findAllergens(foods: Array[Food]): Map[String, Set[String]] =
    val allAllergies = foods.map(_.allergies).reduce(_ union _)
    // For each allergy, find ingredients which are always associated.
    allAllergies
      .map(a => {
        val f = foods
          .filter(_.allergies.contains(a))
          .map(_.ingredients)
          .reduce(_ intersect _)
        a -> f
      })
      .toMap

  def countOccurrences(foods: Array[Food], ings: Set[String]): Int =
    foods.map(_.ingredients.count(ings.contains(_))).sum

  def part1(foods: Input): Int =
    val allIngredients = foods.map(_.ingredients).reduce(_ union _)
    val allergenMap    = findAllergens(foods)
    val allergens      = allergenMap.values.reduce(_ union _)
    val safe           = allIngredients -- allergens
    countOccurrences(foods, safe)

  def removeFromAll[K, V](m: Map[K, Set[V]], value: V): Map[K, Set[V]] =
    m.foldLeft(m)({ case (prev, (k, set)) => prev.updated(k, set - value) })

  @tailrec
  final def reduceAllergens(
      allergenMap: Map[String, Set[String]],
      acc: Map[String, String] = Map.empty
  ): Map[String, String] =
    if (allergenMap.isEmpty) acc
    else {
      val (a, ings) = allergenMap.find(_._2.size == 1).get
      val i         = ings.head
      val nextAcc   = acc + (a -> i)
      var nextMap   = removeFromAll((allergenMap - a), i)
      reduceAllergens(nextMap, nextAcc)
    }

  def part2(foods: Input): String =
    val allergenMap = findAllergens(foods)
    val causes      = reduceAllergens(allergenMap)
    val sorted      = causes.keySet.toList.sorted.map(causes)
    sorted.mkString(",")

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
