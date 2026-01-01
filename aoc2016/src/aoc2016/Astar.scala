package aoc2016

import scala.annotation.tailrec
import aoc2016.Util.Vector2

object Astar:
  case class Result[S](state: S, cost: Long, path: List[S])

  /** Performs an A-star search to find the shortest path from start states to a goal state.
    *
    * @param neighbors
    *   A function that returns the neighbors of a given state and the cost to reach them. The Long value represents the
    *   edge weight (cost) of the transition.
    * @param heuristic
    *   A function that estimates the cost from a given state to the goal. For A* to guarantee the shortest path, this
    *   heuristic must be admissible (i.e., it must never overestimate the true cost). Common heuristics: Manhattan
    *   distance for grids, Euclidean distance for points. If 0 is returned, this behaves like Dijkstra's algorithm.
    * @param isGoal
    *   A predicate that returns true if the state is a goal state.
    * @param starts
    *   The initial states to begin the search from.
    * @return
    *   Some(Result) containing the goal state, total cost, and path if found; None otherwise.
    */
  def search[S](
      neighbors: S => Iterable[(S, Long)],
      heuristic: S => Long,
      isGoal: S => Boolean,
      starts: Iterable[S],
  ): Option[Result[S]] =
    // PriorityQueue stores (f-score, g-score, state):
    // - f-score: how far are we from the end (heuristically)?
    // - g-score: how far are we from the start?

    // Priority ordering: minimum f-score, but tie-break with maximum g-score.
    given Ordering[(Long, Long, S)] with
      def compare(a: (Long, Long, S), b: (Long, Long, S)): Int =
        val fCmp = b._1.compare(a._1) // Min-heap for f
        if fCmp != 0 then fCmp else a._2.compare(b._2) // Max-heap for g

    import scala.collection.{mutable as m}
    val frontier = m.PriorityQueue.empty[(Long, Long, S)]
    val gScores  = m.Map.empty[S, Long]
    val cameFrom = m.Map.empty[S, S]

    starts.foreach: start =>
      gScores(start) = 0L
      frontier.enqueue((heuristic(start), 0L, start))

    @tailrec
    def loop(): Option[S] =
      if frontier.isEmpty then None
      else
        val (_, g, current) = frontier.dequeue()

        // If we found a shorter path to 'current' already, skip this stale entry.
        if g > gScores.getOrElse(current, Long.MaxValue) then loop()
        else if isGoal(current) then Some(current)
        else
          neighbors(current).foreach:
            case (next, weight) =>
              val newG = g + weight
              if newG < gScores.getOrElse(next, Long.MaxValue) then
                gScores(next) = newG
                cameFrom(next) = current
                frontier.enqueue((newG + heuristic(next), newG, next))
          loop()

    @tailrec
    def reconstructPath(curr: S, acc: List[S] = Nil): List[S] =
      cameFrom.get(curr) match
        case Some(prev) => reconstructPath(prev, curr :: acc)
        case None       => curr :: acc

    loop().map(s => Result(s, gScores(s), reconstructPath(s)))
  end search

  /** Performs an A-star search to find the shortest path on a grid from start to goal. This is just a simplified
    * interface on `search` tailored for basic grids; we assume:
    *   - movement on a grid is orthogonal (N,S,E,W only)
    *   - grid cells are either walkable or not walkable
    *   - the cost to walk to a neighbor is simply 1
    *   - there is one start and one goal cell, and these are known in advance
    *
    * @param isWalkable
    *   A function to check if a grid cell is walkable.
    * @param goal
    *   The goal cell.
    * @param start
    *   The starting cell.
    * @return
    *   Some(Result) containing the goal state, total cost, and path if found; None otherwise.
    */
  def gridSearch(isWalkable: Vector2 => Boolean, goal: Vector2, start: Vector2): Option[Result[Vector2]] =
    search[Vector2](
      neighbors = s => s.neighbors4.filter(isWalkable).map(p => (p, 1L)),
      heuristic = s => s.manhattan(goal),
      isGoal = s => s == goal,
      starts = start :: Nil,
    )
