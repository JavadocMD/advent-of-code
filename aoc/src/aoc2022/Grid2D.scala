package aoc2022

import scala.annotation.tailrec

object Grid2D:
  case class Vector2(x: Int, y: Int):
    def minus(other: Vector2) = Vector2(x - other.x, y - other.y)
    def plus(other: Vector2)  = Vector2(x + other.x, y + other.y)
    def manhattanLength       = x.abs + y.abs
    def neighbors = List(
      Vector2(x, y + 1), // up
      Vector2(x, y - 1), // down
      Vector2(x - 1, y), // left
      Vector2(x + 1, y)  // right
    )
  object Vector2:
    val zero = Vector2(0, 0)
  end Vector2

  type Path = List[Vector2]
  object Path:
    def stepLength(path: Path): Int = path.size - 1
  end Path

  type Grid[T] = Map[Vector2, T]
  object Grid:
    def search[T](grid: Grid[T], where: T => Boolean)(start: Vector2): Vector2 =
      @tailrec
      def recurse(open: Set[Vector2], closed: Set[Vector2]): Vector2 =
        open.find(p => where(grid(p))) match
          case Some(p) => p
          case None =>
            val nextClosed = (closed ++ open)
            val nextOpen   = open.flatMap(_.neighbors).filter(p => grid.contains(p) && !nextClosed.contains(p))
            recurse(nextOpen, nextClosed)
      recurse(Set(start), Set.empty)
  end Grid

  object Astar:
    type V = Vector2

    case class State(frontier: Vector[(V, Int)], cameFrom: Map[V, V], score: Map[V, Int]):
      def dequeued(): (V, State) =
        var f = frontier.sortBy(_._2)
        (f.head._1, copy(frontier = f.tail))

    @tailrec
    def reconstructPath(state: State, x: V, path: Path = Nil): Path =
      state.cameFrom.get(x) match
        case Some(y) => reconstructPath(state, y, x :: path)
        case None    => x :: path

    def create(walkableNeighbors: V => List[V], heuristic: V => Int, isDestination: V => Boolean) =
      def consider(s: State, prev: V, curr: V): State =
        val currScore = s.score(prev) + 1
        if currScore >= s.score(curr) then s
        else
          State(
            s.frontier :+ (curr, currScore + heuristic(curr)),
            s.cameFrom.updated(curr, prev),
            s.score.updated(curr, currScore)
          )

      def search(start: V): Path =
        @tailrec
        def recurse(state: State): Path =
          val (curr, s0) = state.dequeued()
          if isDestination(curr) then reconstructPath(state, curr)
          else recurse(walkableNeighbors(curr).foldLeft(s0) { consider(_, curr, _) })

        val f = Vector(start -> 0)
        val c = Map.empty[V, V]
        val s = Map(start -> 0).withDefault(p => Int.MaxValue)
        recurse(State(f, c, s))

      search
  end Astar
