package aoc2019

import utest._

object Day01Test extends TestSuite:
  import Day01._

  val tests = Tests {
    "calculate fuel" - {
      Day01.calculateFuel(1) ==> 0
      Day01.calculateFuel(12) ==> 2
      Day01.calculateFuel(14) ==> 2
      Day01.calculateFuel(1969) ==> 654
      Day01.calculateFuel(100756) ==> 33583
    }

    "calculate fuel's fuel" - {
      Day01.calculateFuelFuel(14) ==> 2
      Day01.calculateFuelFuel(1969) ==> 966
      Day01.calculateFuelFuel(100756) ==> 50346
    }
  }
