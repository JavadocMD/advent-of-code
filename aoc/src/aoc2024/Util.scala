package aoc2024

import scala.annotation.tailrec

object Util:
  case class Vector2(x: Int, y: Int):
    def +(that: Vector2): Vector2     = Vector2(this.x + that.x, this.y + that.y)
    def -(that: Vector2): Vector2     = Vector2(this.x - that.x, this.y - that.y)
    def *(that: Vector2): Vector2     = Vector2(this.x * that.y, this.y * that.y)
    def /(that: Vector2): Vector2     = Vector2(this.x / that.y, this.y / that.y)
    def neighbors: List[Vector2]      = Direction.all.map(this + _)
    def manhattan(that: Vector2): Int = (that.x - this.x).abs + (that.y - this.y).abs
    override def toString(): String   = s"$x,$y"

  object Vector2:
    def s(x: String, y: String): Vector2 = Vector2(x.toInt, y.toInt)

  given Ordering[Vector2] with
    def compare(a: Vector2, b: Vector2): Int =
      val dx = a.x.compareTo(b.x)
      if dx != 0 then dx else a.y.compareTo(b.y)

  object Direction:
    val E   = Vector2(1, 0)
    val S   = Vector2(0, 1)
    val W   = Vector2(-1, 0)
    val N   = Vector2(0, -1)
    val all = List(E, S, W, N)

  type Grid[T] = Map[Vector2, T]

  object Grid:
    def iterate[T](size: Vector2)(f: Vector2 => T): Grid[T] =
      Map.from(
        for
          y <- 0 until size.y
          x <- 0 until size.x
          p = Vector2(x, y)
        yield p -> f(p)
      )

  case class Path(val score: Long, val path: List[Vector2])

  def bfs[T](
      grid: Grid[T],
      isWalkable: Vector2 => Boolean,
      cost: Vector2 => Long,
      start: Vector2,
      finish: Vector2
  ): Option[Path] =
    import scala.collection.immutable.TreeSet

    given Ordering[(Long, Vector2, List[Vector2])] = Ordering.by(x => (x._1, x._2))

    def walkables(p: Vector2) = p.neighbors.filter(isWalkable)

    @tailrec
    def recurse(queue: TreeSet[(Long, Vector2, List[Vector2])], visited: Set[Vector2] = Set.empty): Option[Path] =
      if queue.isEmpty then None
      else
        val (score, current, path) = queue.head
        val rest                   = queue.tail

        if current == finish then Some(Path(score, path.reverse))
        else if visited.contains(current) then recurse(rest, visited)
        else
          val neighbors = walkables(current).filterNot(visited.contains)
          val newPaths  = neighbors.map(p => (score + cost(p), p, p :: path))
          recurse(rest ++ newPaths, visited + current)

    recurse(TreeSet((0L, start, start :: Nil)))
