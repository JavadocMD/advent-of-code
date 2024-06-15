package aoc2022

import scala.annotation.tailrec

import aoc.Day

object Day16 extends Day:
  type Id = String
  case class Room(id: Id, flow: Int, tunnels: List[Id])

  type Input = List[Room]
  def parse(xs: Iterator[String]): Input = xs.map { case s"Valve $id has flow rate=$flow; $_ $_ to $_ $tunnelsStr" =>
    val tunnels = tunnelsStr.split(", ").toList
    Room(id, flow.toInt, tunnels)
  }.toList

  case class RoomMap(
      /** map of rooms by id */
      rooms: Map[Id, Room],
      /** map from starting room to a map containing the best distance to all other rooms */
      routes: Map[Id, Map[Id, Int]],
      /** rooms containing non-zero-flow valves */
      valves: Set[Id]
  )

  // precalculate useful things like pathfinding
  def constructMap(input: Input): RoomMap =
    val rooms       = input.map(r => r.id -> r).toMap
    val valves      = input.filter(r => r.flow > 0).map(r => r.id).toSet
    val tunnels     = rooms.mapValues(_.tunnels).toMap
    val routeSearch = RouteSearch(tunnels)
    val routes      = (valves + "AA").iterator.map { id => id -> routeSearch(id) }.toMap
    RoomMap(rooms, routes, valves)

  // find the best path (the order of valves to open) and the total pressure released by taking it
  def bestPath(map: RoomMap, start: Id, valves: Set[Id], timeAllowed: Int): (List[Id], Int) =
    // each step involves moving to a room with a useful valve and opening it
    // we don't need to track each (empty) room in between
    // we limit our options by only considering the still-closed valves
    // and `valves` has already culled any room with a flow value of 0 -- no point in considering these rooms!

    def recurse(path: List[Id], valvesLeft: Set[Id], timeLeft: Int, totalValue: Int): (List[Id], Int) =
      // recursively consider all plausible options
      // we are finished when we no longer have time to reach another valve or all valves are open
      valvesLeft
        .flatMap { id =>
          val current  = path.head
          val distance = map.routes(current)(id)
          // how much time is left after we traverse there and open the valve?
          val t = timeLeft - distance - 1
          // if `t` is zero or less this option can be skipped
          Option.when(t > 0) {
            // the value of choosing a particular valve (over the life of our simulation)
            // is its flow rate multiplied by the time remaining after opening it
            val value = map.rooms(id).flow * t
            recurse(id :: path, valvesLeft - id, t, totalValue + value)
          }
        }
        .maxByOption(_._2)
        .getOrElse { (path.reverse, totalValue) }
    end recurse
    recurse(start :: Nil, valves, timeAllowed, 0)

  def part1(input: Input) =
    val time   = 30
    val map    = constructMap(input)
    val (p, v) = bestPath(map, "AA", map.valves, time)
    // println(p)
    v
  end part1

  def part2(input: Input) =
    val time = 26
    val map  = constructMap(input)

    // in the optimal solution, the elephant and I will have divided responsibility for switching the valves
    // 15 (useful valves) choose 7 (half) yields only 6435 possible divisions which is a reasonable search space!
    val valvesA = map.valves.toList
      .combinations(map.valves.size / 2)
      .map(_.toSet)

    // NOTE: I assumed an even ditribution of valves would be optimal, and that turned out to be true.
    // However I suppose it's possible an uneven distribution could have been optimal for some graphs.
    // To be safe, you could re-run this using all reasonable values of `n` for `combinations` (1 to 7) and taking the best of those.

    // we can now calculate the efforts separately and sum their values to find the best
    val allPaths = for va <- valvesA yield
      val vb              = map.valves -- va
      val (pathA, scoreA) = bestPath(map, "AA", va, time)
      val (pathB, scoreB) = bestPath(map, "AA", vb, time)
      ((pathA, pathB), scoreA + scoreB)

    val ((pa, pb), v) = allPaths.maxBy(_._2)
    // println(pa)
    // println(pb)
    v
  end part2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))

  // a modified A-star to calculate the best distance to all rooms rather then the best path to a single room
  object RouteSearch:
    case class State(frontier: List[(Id, Int)], score: Map[Id, Int]):
      private def setScore(id: Id, s: Int) = State((id, s + 1) :: frontier, score + (id -> s))

      def dequeued(): (Id, State) =
        var f = frontier.sortBy(_._2)
        (f.head._1, copy(frontier = f.tail))

      def considerEdge(from: Id, to: Id): State =
        val toScore = score(from) + 1
        if toScore >= score(to) then this
        else setScore(to, toScore)
    end State

    object State:
      def initial(start: Id) = State(List((start, 0)), Map(start -> 0).withDefault(_ => Int.MaxValue))

    def apply(neighbors: Id => List[Id])(start: Id): Map[Id, Int] =
      var state = State.initial(start)
      while state.frontier.nonEmpty do
        val (curr, currState) = state.dequeued()
        state = neighbors(curr)
          .foldLeft(currState) { (s, n) =>
            s.considerEdge(curr, n)
          }
      state.score

  end RouteSearch
