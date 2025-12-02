package aoc2022

import utest._

object Day25Test extends TestSuite:
  import Day25._

  val tests = Tests {
    "parse SNAFU" - {
      fromSnafu("1=-0-2") ==> 1747
      fromSnafu("12111") ==> 906
      fromSnafu("2=0=") ==> 198
      fromSnafu("21") ==> 11
      fromSnafu("2=01") ==> 201
      fromSnafu("111") ==> 31
      fromSnafu("20012") ==> 1257
      fromSnafu("112") ==> 32
      fromSnafu("1=-1=") ==> 353
      fromSnafu("1-12") ==> 107
      fromSnafu("12") ==> 7
      fromSnafu("1=") ==> 3
      fromSnafu("122") ==> 37
    }

    "format SNAFU" - {
      toSnafu(1747) ==> "1=-0-2"
      toSnafu(906) ==> "12111"
      toSnafu(198) ==> "2=0="
      toSnafu(11) ==> "21"
      toSnafu(201) ==> "2=01"
      toSnafu(31) ==> "111"
      toSnafu(1257) ==> "20012"
      toSnafu(32) ==> "112"
      toSnafu(353) ==> "1=-1="
      toSnafu(107) ==> "1-12"
      toSnafu(7) ==> "12"
      toSnafu(3) ==> "1="
      toSnafu(37) ==> "122"
    }

    "sum SNAFU" - {
      snafuSum("0", "0") ==> "0"
      snafuSum("0", "1") ==> "1"
      snafuSum("1", "0") ==> "1"
      snafuSum("1", "1") ==> "2"
      snafuSum("1", "2") ==> "1="
      snafuSum("2", "2") ==> "1-"
      snafuSum("2", "1=") ==> "10"
      snafuSum("1=", "2") ==> "10"
      snafuSum("1-", "2") ==> "11"
      snafuSum("1=", "1=") ==> "11"
    }
  }
