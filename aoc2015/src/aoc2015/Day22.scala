package aoc2015

import scala.annotation.tailrec
import aoc.Day
import scala.math.max

object Day22 extends Day:

  case class Boss(hitPoints: Int, damage: Int)

  type Input = Boss

  def parse(xs: List[String]): Input =
    xs.mkString(" ") match
      case s"Hit Points: $hp Damage: $dmg" => Boss(hp.toInt, dmg.toInt)

  lazy val input = parse(loadInput().toList)

  sealed trait Spell(val cost: Int, val duration: Int)
  case object MagicMissile extends Spell(53, 0)
  case object Drain        extends Spell(73, 0)
  case object Shield       extends Spell(113, 6)
  case object Poison       extends Spell(173, 6)
  case object Recharge     extends Spell(229, 5)
  case object HardMode     extends Spell(0, 0)

  val spells = List(
    MagicMissile,
    Drain,
    Shield,
    Poison,
    Recharge,
  )

  val boss = input

  case class GameState(bossHp: Int, yourHp: Int, yourArmor: Int, yourMana: Int, effective: Map[Spell, Int], spent: Int):
    def isWin: Boolean = bossHp <= 0 && yourHp > 0

    def canCast: Iterator[Spell] =
      spells.iterator.takeWhile(_.cost <= this.yourMana).filterNot(this.effective.contains)

    def doEffects(turn: "your" | "boss"): GameState =
      val active = this.effective.contains
      val armor  = if active(Shield) then 7 else 0
      val bossHp = if active(Poison) then this.bossHp - 3 else this.bossHp
      val mana   = if active(Recharge) then this.yourMana + 101 else this.yourMana
      val yourHp = if active(HardMode) && turn == "your" then this.yourHp - 1 else this.yourHp

      val nextEffective = this.effective.collect:
        case spell -> turns if turns > 1 => spell -> (turns - 1)

      this.copy(
        bossHp = bossHp,
        yourHp = yourHp,
        yourArmor = armor,
        yourMana = mana,
        effective = nextEffective,
      )

    def doCast(spell: Spell): GameState = spell match
      case MagicMissile =>
        this.copy(
          bossHp = this.bossHp - 4,
          yourMana = this.yourMana - spell.cost,
          spent = this.spent + spell.cost,
        )
      case Drain =>
        this.copy(
          bossHp = this.bossHp - 2,
          yourHp = this.yourHp + 2,
          yourMana = this.yourMana - spell.cost,
          spent = this.spent + spell.cost
        )
      case _ =>
        this.copy(
          effective = this.effective.updated(spell, spell.duration),
          yourMana = this.yourMana - spell.cost,
          spent = this.spent + spell.cost
        )

    def doBossAttack: GameState =
      this.copy(yourHp = this.yourHp - max(1, boss.damage - this.yourArmor))

  object GameState:
    val normal = GameState(boss.hitPoints, 50, 0, 500, Map.empty, 0)
    val hard   = GameState(boss.hitPoints, 50, 0, 500, Map(HardMode -> Int.MaxValue), 0)

  // what's the least amount of mana we can spend and still win?
  def playLeast(boss: Boss, initial: GameState): Option[GameState] =
    def takeTurns(state: GameState): Option[GameState] =
      val s0 = state.doEffects("your")
      if s0.bossHp <= 0 then Some(s0)
      else if s0.yourHp <= 0 then None
      else
        s0.canCast
          .flatMap: spell =>
            val s1 = s0.doCast(spell)
            if s1.bossHp <= 0 then Some(s1)
            else
              val s2 = s1.doEffects("boss")
              if s2.bossHp <= 0 then Some(s2)
              else
                val s3 = s2.doBossAttack
                if s3.yourHp <= 0 then None
                else takeTurns(s3)
          .minByOption(_.spent)

    takeTurns(initial)

  lazy val part1 = playLeast(input, GameState.normal).get.spent

  lazy val part2 = playLeast(input, GameState.hard).get.spent

  final def main(args: Array[String]): Unit =
    input
    solveP1(() => part1)
    solveP2(() => part2)
