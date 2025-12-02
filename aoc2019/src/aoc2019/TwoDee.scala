package aoc2019

import scala.reflect.ClassTag
import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec

object TwoDee:
  import scala.math.abs

  // Directionality: East is +x, North is +y

  sealed trait Direction
  object Direction:
    case object North extends Direction
    case object South extends Direction
    case object West  extends Direction
    case object East  extends Direction

    def turnRight(d: Direction) = d match
      case North => East
      case East  => South
      case South => West
      case West  => North

    def turnLeft(d: Direction) = d match
      case North => West
      case West  => South
      case South => East
      case East  => North

  trait Directionality:
    def toVector: Vector

  object Directionality:
    implicit class OriginBottomLeft(self: Direction) extends Directionality:
      def toVector: Vector = self match {
        case Direction.North => Vector(0, +1)
        case Direction.East  => Vector(+1, 0)
        case Direction.South => Vector(0, -1)
        case Direction.West  => Vector(-1, 0)
      }

    implicit class OriginTopLeft(self: Direction) extends Directionality:
      def toVector: Vector = self match {
        case Direction.North => Vector(0, -1)
        case Direction.East  => Vector(+1, 0)
        case Direction.South => Vector(0, +1)
        case Direction.West  => Vector(-1, 0)
      }

  type Neighbor = (Direction, Vector)

  case class Vector(x: Int, y: Int):
    def +(that: Vector) = Vector(this.x + that.x, this.y + that.y)

    def -(that: Vector) = Vector(this.x - that.x, this.y - that.y)

    def scale(f: Int) = Vector(this.x * f, this.y * f)

    def distance(that: Vector): Int = abs(this.x - that.x) + abs(this.y - that.y)

    def neighbors: Seq[Vector] = Seq(
      Vector(x, y + 1),
      Vector(x, y - 1),
      Vector(x + 1, y),
      Vector(x - 1, y)
    )

    def neighborsWithDir: Seq[Neighbor] = Seq(
      (Direction.North, Vector(x, y + 1)),
      (Direction.South, Vector(x, y - 1)),
      (Direction.East, Vector(x + 1, y)),
      (Direction.West, Vector(x - 1, y))
    )

  object Vector:
    val zero = Vector(0, 0)

  type Grid[T] = Map[Vector, T]

  object Grid:
    def create[T](unknownValue: T): Grid[T] = Map.empty[Vector, T].withDefaultValue(unknownValue)

    def parse[T](unknownValue: T, parseChar: Char => T)(xs: Array[String]): Grid[T] = {
      val cells = for {
        (row, y)  <- Stream.from(xs).zip(Stream.from(0))
        (char, x) <- Stream.from(row).zip(Stream.from(0))
      } yield (Vector(x, y) -> parseChar(char))
      Map.from(cells).withDefaultValue(unknownValue)
    }

    def toArray[T: ClassTag](grid: Grid[T]): Array[Array[T]] = {
      val xs = grid.map({ case (v, _) => v.x })
      val ys = grid.map({ case (v, _) => v.y })

      val xOffset = xs.min
      val yOffset = ys.min

      val width  = xs.max - xOffset + 1
      val height = ys.max - yOffset + 1

      Array.tabulate[T](height, width)({ case (y, x) =>
        grid(Vector(x + xOffset, y + yOffset))
      })
    }

    def draw[T: ClassTag](grid: Grid[T], drawCell: T => String): String = {
      toArray(grid).reverseIterator.map(row => row.map(drawCell).mkString).mkString("\n")
    }

    def drawInverted[T: ClassTag](grid: Grid[T], drawCell: T => String): String = {
      toArray(grid).map(row => row.map(drawCell).mkString).mkString("\n")
    }
  end Grid

  type DistanceMap = Map[Vector, Int]    // for each cell, how far have I come?
  type FlowMap     = Map[Vector, Vector] // for each cell, where did I come from?

  case class BreadthFirstSearch[T](grid: Grid[T], distance: DistanceMap, flow: FlowMap)

  object BreadthFirstSearch:
    def create[T](grid: Grid[T], isWalkable: T => Boolean, start: Vector): BreadthFirstSearch[T] =

      @tailrec
      def recurse(frontier: Seq[Vector], distance: DistanceMap, flow: FlowMap): BreadthFirstSearch[T] =
        frontier match
          case Nil =>
            // When frontier is empty, we are done.
            BreadthFirstSearch(grid, distance, flow)
          case head :: tail =>
            // Add each neighbor which is walkable and isn't already in the distance map.
            val ns = head.neighbors.filter(v => isWalkable(grid(v)) && !distance.contains(v))
            val d  = distance(head) + 1
            recurse(tail ++ ns, distance ++ ns.map(v => v -> d), flow ++ ns.map(v => v -> head))
      end recurse

      // Frontier begins with our starting location which has cost 0 and no flow entry.
      recurse(Seq(start), Map((start, 0)), Map.empty)

  object AStar:
    // NOTE: AStar is kind of overkill for pathfinding on a grid. And on moderately sized grids, slower than BFS.

    case class CellScore(cell: Vector, score: Int)
    object CellScore:
      object ScoreOrdering extends Ordering[CellScore]:
        def compare(x: CellScore, y: CellScore): Int = x.score.compare(y.score)

    class OpenSet:
      private[this] var cells: Set[Vector]              = Set.empty
      private[this] val queue: PriorityQueue[CellScore] = PriorityQueue.empty(CellScore.ScoreOrdering)

      // Add the cell to the open set with the given score, unless it's already there.
      def addUnique(cell: Vector, score: Int): Unit =
        if (!cells.contains(cell)) {
          cells += cell
          queue += CellScore(cell, score)
        }

      def dequeue: Vector =
        val CellScore(cell, _) = queue.dequeue
        cells -= cell
        cell

      def contains(cell: Vector): Boolean = cells.contains(cell)

      def nonEmpty: Boolean = cells.nonEmpty

    end OpenSet

    @tailrec
    final def tracePath(cameFrom: Map[Vector, Vector], goal: Vector, path: List[Vector] = List.empty): List[Vector] =
      cameFrom.get(goal) match
        case None    => path
        case Some(v) => tracePath(cameFrom, v, v :: path)

    def findPath[T](grid: Grid[T], isWalkable: T => Boolean, start: Vector, goal: Vector): Seq[Vector] =
      // implements A* pathfinding where
      // the cost to traverse between cells is one, and
      // the heuristic is manhattan distance
      import scala.collection.mutable.{Map, Set}

      type CellScore = (Int, Vector)
      object ScoreOrdering extends Ordering[CellScore]:
        def compare(x: CellScore, y: CellScore): Int = x._1.compare(y._1)

      // the set of nodes currently under consideration; complete when empty
      val openSet = new OpenSet
      // the best path from start to this cell (key) came through this cell (value)
      val cameFrom = Map.empty[Vector, Vector]
      // cost to get here (g score)
      val cost = Map.empty[Vector, Int].withDefaultValue(Int.MaxValue)
      // cost to here plus the estimated cost from here to goal (f score)
      val estimate = Map.empty[Vector, Int].withDefaultValue(Int.MaxValue)

      if (isWalkable(grid(start))) {
        val e = start.distance(goal)
        cost(start) = 0
        estimate(start) = e
        openSet.addUnique(start, e)
      }

      while (openSet.nonEmpty) {
        // Consider our closest-estimated cell in the open set.
        val current = openSet.dequeue

        if (current == goal) {
          // Success! Reconstruct path and return.
          return tracePath(cameFrom.toMap, goal)
        }

        // Add neighbors to the open set if they are walkable with a better g-score
        current.neighbors.foreach({ case neighbor =>
          val t = grid(neighbor)
          val c = cost(current) + 1
          if (current != neighbor && isWalkable(t) && c < cost(neighbor)) {
            // This path is better than the one we've got, update.
            val e = c + neighbor.distance(goal)
            cameFrom(neighbor) = current
            cost(neighbor) = c
            estimate(neighbor) = e
            openSet.addUnique(neighbor, e)
          }
        })
      }

      // Failed result is empty.
      Seq.empty[Vector]
    end findPath
  end AStar
