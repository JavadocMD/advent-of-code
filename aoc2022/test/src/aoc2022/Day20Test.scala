package aoc2022

import utest._

object Day20Test extends TestSuite:
  import Day20._
  val tests = Tests {
    "mix test list" - {
      val list = List(
        new Number(1),
        new Number(2),
        new Number(-3),
        new Number(3),
        new Number(-2),
        new Number(0),
        new Number(4)
      )

      val exp = List(1, 2, -3, 4, 0, 3, -2).map(_.toLong)
      val act = mixAll(list, list)
      assertListMatch(exp, act)
    }

    "mix test list part 2" - {
      val list = List(
        new Number(1),
        new Number(2),
        new Number(-3),
        new Number(3),
        new Number(-2),
        new Number(0),
        new Number(4)
      ).map(n => new Number(n.value * 811589153L))

      val exps = List(
        List(811589153L, 1623178306L, -2434767459L, 2434767459L, -1623178306L, 0L, 3246356612L),
        List(0L, -2434767459L, 3246356612L, -1623178306L, 2434767459L, 1623178306L, 811589153L),
        List(0L, 2434767459L, 1623178306L, 3246356612L, -2434767459L, -1623178306L, 811589153L),
        List(0L, 811589153L, 2434767459L, 3246356612L, 1623178306L, -1623178306L, -2434767459L),
        List(0L, 1623178306L, -2434767459L, 811589153L, 2434767459L, 3246356612L, -1623178306L),
        List(0L, 811589153L, -1623178306L, 1623178306L, -2434767459L, 3246356612L, 2434767459L),
        List(0L, 811589153L, -1623178306L, 3246356612L, -2434767459L, 1623178306L, 2434767459L),
        List(0L, -2434767459L, 2434767459L, 1623178306L, -1623178306L, 811589153L, 3246356612L),
        List(0L, 1623178306L, 3246356612L, 811589153L, -2434767459L, 2434767459L, -1623178306L),
        List(0L, 811589153L, 1623178306L, -2434767459L, 3246356612L, 2434767459L, -1623178306L),
        List(0L, -2434767459L, 1623178306L, 3246356612L, -1623178306L, 2434767459L, 811589153L)
      )

      val acts = (0 until 10).toList
        .scanLeft(list) { (xs, _) => mixAll(list, xs) }

      assertListMatch(exps(0), acts(0))
      println("pass 0")
      assertListMatch(exps(1), acts(1))
      println("pass 1")
      assertListMatch(exps(2), acts(2))
      println("pass 2")
      assertListMatch(exps(3), acts(3))
      println("pass 3")
      assertListMatch(exps(4), acts(4))
      println("pass 4")
      assertListMatch(exps(5), acts(5))
      println("pass 5")
      assertListMatch(exps(6), acts(6))
      println("pass 6")
      assertListMatch(exps(7), acts(7))
      println("pass 7")
      assertListMatch(exps(8), acts(8))
      println("pass 8")
      assertListMatch(exps(9), acts(9))
      println("pass 9")
      assertListMatch(exps(10), acts(10))
      println("pass 10")
    }
  }

  def assertListMatch(expected: List[Long], actual: List[Number]): Unit =
    actual.size ==> expected.size
    val n = actual.size
    val i = expected.indexOf(0)
    val j = actual.indexWhere(_.value == 0)
    (0 until n).foreach { di =>
      val e = expected((i + di) % n)
      val a = actual((j + di) % n)
      a.value ==> e
    }
