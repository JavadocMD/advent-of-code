package aoc2020

import utest._

object Day21Test extends TestSuite:
  import Day21._

  val input = """
    |mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
    |trh fvjkl sbzzf mxmxvkd (contains dairy)
    |sqjhc fvjkl (contains soy)
    |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin.trim.linesIterator.toArray

  val tests = Tests {
    "parse foods" - {
      input.map(Food.from).head ==> Food(
        Set("mxmxvkd", "kfcds", "sqjhc", "nhms"),
        Set("dairy", "fish")
      )
    }

    "sort ingredients" - {
      val foods       = input.map(Food.from)
      val allergenMap = findAllergens(foods)
      val allergens   = allergenMap.values.reduce(_ union _)
      allergens ==> Set("mxmxvkd", "sqjhc", "fvjkl")
    }

    "count occurrences" - {
      val foods = input.map(Food.from)
      countOccurrences(foods, Set("kfcds", "nhms", "sbzzf", "trh")) ==> 5
    }

    "reduce allergens" - {
      val foods       = input.map(Food.from)
      val allergenMap = findAllergens(foods)
      reduceAllergens(allergenMap) ==> Map(
        "dairy" -> "mxmxvkd",
        "fish"  -> "sqjhc",
        "soy"   -> "fvjkl"
      )
    }

    "produce canonical list" - {
      val foods       = input.map(Food.from)
      val allergenMap = findAllergens(foods)
      val causes      = reduceAllergens(allergenMap)
      val sorted      = causes.keySet.toList.sorted.map(causes)
      sorted.mkString(",") ==> "mxmxvkd,sqjhc,fvjkl"
    }
  }
