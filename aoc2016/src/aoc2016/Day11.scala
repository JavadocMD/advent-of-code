package aoc2016

import scala.annotation.tailrec
import aoc.Day
import aoc2016.Astar

object Day11 extends Day:

  enum Item:
    case PmG, PmM, CoG, CoM, CmG, CmM, RuG, RuM, PuG, PuM, EeG, EeM, DiG, DiM, Elevator

    val isGen: Boolean  = this.toString.endsWith("G")
    val isChip: Boolean = this.toString.endsWith("M")

  object Item:
    def dual(item: Item): Item = item match
      case PmG      => PmM
      case PmM      => PmG
      case CoG      => CoM
      case CoM      => CoG
      case CmG      => CmM
      case CmM      => CmG
      case RuG      => RuM
      case RuM      => RuG
      case PuG      => PuM
      case PuM      => PuG
      case EeG      => EeM
      case EeM      => EeG
      case DiG      => DiM
      case DiM      => DiG
      case Elevator => Elevator

  import Item._

  val adjacentFloors: Map[Int, List[Int]] = Map(
    1 -> List(2),
    2 -> List(1, 3),
    3 -> List(2, 4),
    4 -> List(3),
  )

  case class State(items: Map[Item, Int]):
    def isFinal: Boolean = items.forall((_, floor) => floor == 4)

    lazy val heuristic: Int = items.map((_, floor) => 4 - floor).sum

    def isValid: Boolean =
      items
        .groupBy(_._2)
        .view
        .mapValues(_.keySet)
        .forall: (_, floorItems) =>
          !floorItems.exists(_.isGen) ||
            !floorItems.exists(chip => chip.isChip && !floorItems.contains(Item.dual(chip)))

    def neighbors: List[State] =
      val currFloor = items(Elevator)
      val currItems = (items.filter((_, floor) => floor == currFloor).keySet - Elevator).toList

      val states = for
        dst      <- adjacentFloors(currFloor)
        carrying <- currItems.combinations(2) ++ currItems.combinations(1)
      yield
        val nextItems = (Elevator :: carrying).foldLeft(items):
          case (acc, i) => acc.updated(i, dst)
        State(nextItems)
      states.filter(_.isValid)

    // Key optimization!
    //
    // - Straight-forward A-star takes ~2 seconds for Part 1 and an indefinitely long time for Part 2:
    //   there's too many states.
    // - But observe the items are "fungible" in a way. It doesn't really matter if we
    //   carry cobalt to the top floor before plutonium, or vice versa.
    //   All we care about is the _number_ of steps involved.
    // - So we save a lot of duplication by collapsing functionally equivalent states,
    //   and we can do this by tracking the pairs of (chip-floor, gen-floor) agnostic
    //   to which element each pair corresponds to.
    // - And so the A-star algorithm below is able to leverage this object equality (via its priority queue)
    //   to avoid processing duplicative states.
    //
    // For example, these states are equivalent:
    // - Floor 1: {CoG,CoM}, Floor 2: {PuG,PuM} ==> canonically: [(1,1), (2,2)]
    // - Floor 1: {PuG,PuM}, Floor 2: {CoG,CoM} ==> canonically: [(1,1), (2,2)]

    lazy val canonical: (Int, List[(Int, Int)]) =
      val pairs = items.view
        .filter(_._1.isChip)
        .map:
          case (chip, chipFloor) =>
            val genFloor = items(Item.dual(chip))
            (chipFloor, genFloor)
        .toList
        .sorted
      (items(Elevator), pairs)

    override def hashCode: Int = canonical.hashCode
    override def equals(obj: Any): Boolean = obj match
      case other: State => canonical == other.canonical
      case _            => false

  object State:
    val initial = State(
      Map(
        Elevator -> 1,
        PmG      -> 1,
        PmM      -> 1,
        CoG      -> 2,
        CmG      -> 2,
        RuG      -> 2,
        PuG      -> 2,
        CoM      -> 3,
        CmM      -> 3,
        RuM      -> 3,
        PuM      -> 3,
      )
    )

    val initial2 = State(
      Map(
        Elevator -> 1,
        PmG      -> 1,
        PmM      -> 1,
        EeG      -> 1,
        EeM      -> 1,
        DiG      -> 1,
        DiM      -> 1,
        CoG      -> 2,
        CmG      -> 2,
        RuG      -> 2,
        PuG      -> 2,
        CoM      -> 3,
        CmM      -> 3,
        RuM      -> 3,
        PuM      -> 3,
      )
    )
  end State

  lazy val input = loadInput()

  val search = Astar.search[State](
    neighbors = _.neighbors.map((_, 1L)),
    heuristic = _.heuristic,
    isGoal = _.isFinal,
    _
  )

  lazy val part1 = search(State.initial :: Nil).get.cost

  lazy val part2 = search(State.initial2 :: Nil).get.cost

  final def main(args: Array[String]): Unit =
    input // eval input before timing starts
    solveP1(() => part1)
    solveP2(() => part2)
