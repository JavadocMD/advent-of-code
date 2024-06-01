package aoc2023

sealed trait Dir
case object Up    extends Dir
case object Down  extends Dir
case object Left  extends Dir
case object Right extends Dir

def turns(dir: Dir): List[Dir] = dir match
  case Up    => Left :: Right :: Nil
  case Down  => Right :: Left :: Nil
  case Left  => Up :: Down :: Nil
  case Right => Down :: Up :: Nil

case class Point(x: Int, y: Int):
  def manhattanDist(other: Point): Int =
    math.abs(other.x - this.x) + math.abs(other.y - this.y)

  def up    = copy(y = y - 1)
  def down  = copy(y = y + 1)
  def left  = copy(x = x - 1)
  def right = copy(x = x + 1)

  def adjacent: List[Point] = List(up, down, left, right)

  def go(dir: Dir): Point = dir match
    case Up    => up
    case Down  => down
    case Left  => left
    case Right => right

  def goN(dir: Dir, steps: Int): Point = dir match
    case Up    => copy(y = y - steps)
    case Down  => copy(y = y + steps)
    case Left  => copy(x = x - steps)
    case Right => copy(x = x + steps)

object Point:
  val zero = Point(0, 0)

class Grid(input: List[String]):
  val height = input.size
  val width  = input(0).size
  val size   = (width, height)

  def pointsIterator: Iterator[(Point, Char)] =
    for
      (line, j) <- input.iterator.zipWithIndex
      (char, i) <- line.zipWithIndex
    yield (Point(i, j), char)

  def contains(p: Point): Boolean =
    p.x >= 0 && p.y >= 0 && p.x < width && p.y < height
