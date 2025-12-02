package aoc2021

import utest._
import Binary._

object UtilTest extends TestSuite:

  val tests = Tests {
    "Binary.parse" - {
      parse("0") ==> 0
      parse("1") ==> 1
      parse("10") ==> 2
      parse("11") ==> 3
      parse("1100101001") ==> 809
    }

    "Binary.invert" - {
      given BitSize(8)
      invert(parse("10101010")) ==> parse("01010101")
      invert(parse("00000000")) ==> parse("11111111")
      invert(parse("11111111")) ==> parse("00000000")
      invert(parse("1111")) ==> parse("11110000")
    }

    "Binary.onesCount" - {
      given BitSize(8)
      onesCount(parse("10101010")) ==> 4
      onesCount(parse("00000000")) ==> 0
      onesCount(parse("11111111")) ==> 8
      onesCount(parse("11111")) ==> 5
    }

    "Binary.zerosCount" - {
      given BitSize(8)
      zerosCount(parse("10101010")) ==> 4
      zerosCount(parse("00000000")) ==> 8
      zerosCount(parse("11111111")) ==> 0
      zerosCount(parse("11111")) ==> 3
    }

    "Binary.rotate" - {
      given BitSize(4)
      rotate(parse("1111")) ==> parse("1111")
      rotate(parse("0011")) ==> parse("1100")
      rotate(parse("1010")) ==> parse("0101")
      rotate(parse("1000")) ==> parse("0001")
    }
  }
