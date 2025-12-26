package aoc2015

import scala.annotation.tailrec
import aoc.Day
import scala.math.{min, max, ceil}

object Day21 extends Day:

  case class Item(name: String, cost: Int, damage: Int, armor: Int)
  case class Character(name: String, cost: Int, damage: Int, armor: Int, items: List[Item])

  type Input = Character

  def parse(xs: List[String]): Input = xs.mkString(" ") match
    case s"Hit Points: 100 Damage: $dmg Armor: $arm" => Character("boss", 0, dmg.toInt, arm.toInt, Nil)

  lazy val input = parse(loadInput().toList)

  val weapons = List(
    Item("Dagger", 8, 4, 0),
    Item("Shortsword", 10, 5, 0),
    Item("Warhammer", 25, 6, 0),
    Item("Longsword", 40, 7, 0),
    Item("Greataxe", 74, 8, 0),
  )

  val armor = List(
    Item("None", 0, 0, 0),
    Item("Leather", 13, 0, 1),
    Item("Chainmail", 31, 0, 2),
    Item("Splintmail", 53, 0, 3),
    Item("Bandedmail", 75, 0, 4),
    Item("Platemail", 102, 0, 5),
  )

  val rings = List(
    Item("None", 0, 0, 0),
    Item("None", 0, 0, 0),
    Item("Damage +1", 25, 1, 0),
    Item("Damage +2", 50, 2, 0),
    Item("Damage +3", 100, 3, 0),
    Item("Defense +1", 20, 0, 1),
    Item("Defense +2", 40, 0, 2),
    Item("Defense +3", 80, 0, 3),
  )

  def characterBuilds =
    for
      w <- weapons.iterator
      a <- armor.iterator
      case r1 :: r2 :: Nil <- rings.combinations(2)
      items = w :: a :: r1 :: r2 :: Nil
    yield Character(
      name = "you",
      cost = items.map(_.cost).sum,
      armor = items.map(_.armor).sum,
      damage = items.map(_.damage).sum,
      items = items,
    )

  def winBattle(boss: Character, you: Character): Boolean =
    val bossAttack = max(1, boss.damage - you.armor)
    val yourAttack = max(1, you.damage - boss.armor)
    val bossTurns  = 100 / yourAttack + (if 100 % yourAttack > 0 then 1 else 0)
    val yourTurns  = 100 / bossAttack + (if 100 % bossAttack > 0 then 1 else 0)
    yourTurns >= bossTurns

  lazy val part1 = characterBuilds.filter(winBattle(input, _)).minBy(_.cost).cost

  lazy val part2 = characterBuilds.filterNot(winBattle(input, _)).maxBy(_.cost).cost

  final def main(args: Array[String]): Unit =
    input
    solveP1(() => part1)
    solveP2(() => part2)
