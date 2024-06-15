package aoc2022

import scala.annotation.tailrec

def splitGroups[T](xs: Array[String], mapping: String => T = identity): Vector[Vector[T]] =
  @tailrec
  def recurse(rows: List[String], group: Vector[T], all: Vector[Vector[T]]): Vector[Vector[T]] =
    rows match
      case Nil          => all :+ group
      case "" :: tail   => recurse(tail, Vector.empty, all :+ group)
      case head :: tail => recurse(tail, group :+ mapping(head), all)
  recurse(xs.toList, Vector.empty, Vector.empty)

def memoized[A, B](f: A => B): A => B =
  val memory = scala.collection.mutable.Map.empty[A, B]
  a => memory.getOrElseUpdate(a, f(a))

object IntP:
  def unapply(s: String): Option[Int] = try Some(s.toInt)
  catch { case _: NumberFormatException => None }

object LongP:
  def unapply(s: String): Option[Long] = try Some(s.toLong)
  catch { case _: NumberFormatException => None }
