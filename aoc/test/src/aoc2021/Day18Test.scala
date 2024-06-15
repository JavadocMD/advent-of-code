package aoc2021

import utest._

object Day18Test extends TestSuite:
  import Day18._, Tree._, Side._

  val tests = Tests {
    "parse" - {
      val x1 = "[1,1]"
      val t1 = Tree.parse(x1)
      t1 ==> Pair(Num(1), Num(1))
      Tree.toString(t1) ==> x1

      val x2 = "[[[[5,0],[7,4]],[5,5]],[6,6]]"
      val t2 = Tree.parse(x2)
      t2 ==> Pair(
        Pair(
          Pair(
            Pair(Num(5), Num(0)),
            Pair(Num(7), Num(4))
          ),
          Pair(Num(5), Num(5))
        ),
        Pair(Num(6), Num(6))
      )
      Tree.toString(t2) ==> x2
    }

    "replace" - {
      val x0 =
        Pair(
          Pair(Num(1), Num(2)),
          Pair(Num(3), Num(4))
        )

      Tree.replace(x0, Left :: Right :: Nil, Num(9)) ==>
        Pair(
          Pair(Num(1), Num(9)),
          Pair(Num(3), Num(4))
        )

      Tree.replace(x0, Left :: Nil, Num(9)) ==>
        Pair(
          Num(9),
          Pair(Num(3), Num(4))
        )

      Tree.replace(x0, Right :: Nil, x0) ==>
        Pair(
          Pair(Num(1), Num(2)),
          Pair(
            Pair(Num(1), Num(2)),
            Pair(Num(3), Num(4))
          )
        )
    }

    "leftNum" - {
      val x0 =
        Pair(
          Pair(Num(1), Num(2)),
          Pair(Num(3), Num(4))
        )
      leftNum(x0, Left :: Left :: Nil) ==> None
      leftNum(x0, Left :: Right :: Nil) ==> Some(Left :: Left :: Nil)
      leftNum(x0, Right :: Left :: Nil) ==> Some(Left :: Right :: Nil)
      leftNum(x0, Right :: Right :: Nil) ==> Some(Right :: Left :: Nil)
    }

    "rightNum" - {
      val x0 =
        Pair(
          Pair(Num(1), Num(2)),
          Pair(Num(3), Num(4))
        )
      rightNum(x0, Left :: Left :: Nil) ==> Some(Left :: Right :: Nil)
      rightNum(x0, Left :: Right :: Nil) ==> Some(Right :: Left :: Nil)
      rightNum(x0, Right :: Left :: Nil) ==> Some(Right :: Right :: Nil)
      rightNum(x0, Right :: Right :: Nil) ==> None
    }

    "explode" - {
      val in0 = Tree.parse("[[[[[9,8],1],2],3],4]")
      val ex0 = Tree.parse("[[[[0,9],2],3],4]")
      explode(in0) ==> Some(ex0)

      val in1 = Tree.parse("[7,[6,[5,[4,[3,2]]]]]")
      val ex1 = Tree.parse("[7,[6,[5,[7,0]]]]")
      explode(in1) ==> Some(ex1)

      val in2 = Tree.parse("[[6,[5,[4,[3,2]]]],1]")
      val ex2 = Tree.parse("[[6,[5,[7,0]]],3]")
      explode(in2) ==> Some(ex2)

      val in3 = Tree.parse("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")
      val ex3 = Tree.parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
      explode(in3) ==> Some(ex3)

      val in4 = Tree.parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
      val ex4 = Tree.parse("[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
      explode(in4) ==> Some(ex4)
    }

    "add" - {
      val a = Tree.parse("[[[[4,3],4],4],[7,[[8,4],9]]]")
      val b = Tree.parse("[1,1]")
      add(a, b) ==> Tree.parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
    }
  }
