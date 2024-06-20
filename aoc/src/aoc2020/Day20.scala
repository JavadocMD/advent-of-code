package aoc2020

import scala.io.Source
import scala.annotation.tailrec
import scala.reflect.ClassTag

import aoc.Day

object Day20 extends Day:
  import scala.collection.{mutable => m}

  type Input = Array[Tile]

  // 144 tiles; image is 12x12 tiles, each tile is 10x10 pixels
  def parse(xs: Array[String]): Input = toChunks(xs).map(Tile.from)

  def inverse(n: Int): Int =
    var m      = n
    var result = 0
    for { i <- 0 until 10 } {
      result = (result << 1) | (m & 1)
      m = m >>> 1
    }
    result

  // Tile rotation (counter-clockwise).
  sealed trait Rotation
  object Rotation:
    case object R0   extends Rotation
    case object R90  extends Rotation
    case object R180 extends Rotation
    case object R270 extends Rotation

    val all = Seq(R0, R90, R180, R270)

  // Tile vertically flipped (before rotations).
  type Flip = Boolean
  object Flip:
    val all = Seq(false, true)

  class Tile(
      val id: Int,
      val lines: Array[String],
      val edges: Seq[Int]
  ):
    def getEdge(i: Int, rotation: Rotation, flipped: Boolean): Int = {
      val e = if (!flipped) {
        rotation match {
          case Rotation.R0   => (i + 0) % 4
          case Rotation.R90  => (i + 1) % 4
          case Rotation.R180 => (i + 2) % 4
          case Rotation.R270 => (i + 3) % 4
        }
      } else {
        rotation match {
          case Rotation.R0   => (5 - i) % 4 + 4
          case Rotation.R90  => (4 - i) % 4 + 4
          case Rotation.R180 => (3 - i) % 4 + 4
          case Rotation.R270 => (6 - i) % 4 + 4
        }
      }
      edges(e)
    }

    override def toString = s"Tile(${id.toString})"
  end Tile

  object Tile:
    def parseEdge(lines: Array[String], ijSeq: Seq[(Int, Int)]): Int =
      var result = 0
      for { (i, j) <- ijSeq } {
        val b = if (lines(j)(i) == '#') 1 else 0
        result = (result << 1) | b
      }
      result

    // Edges read clockwise.
    private val top    = (0 until 10).map(i => (i, 0))
    private val right  = (0 until 10).map(j => (9, j))
    private val bottom = (9 to 0 by -1).map(i => (i, 9))
    private val left   = (9 to 0 by -1).map(j => (0, j))

    def from(xs: Array[String]): Tile =
      val lines = xs.tail
      val id    = xs.head.split(' ')(1).dropRight(1).toInt
      val es    = Seq(top, right, bottom, left).map(parseEdge(lines, _))
      val esi   = es.map(inverse)
      new Tile(id, lines, es ++ esi)

  end Tile

  case class Match(tile1: Int, tile2: Int, edge1: Int)
  type Matches = m.MultiDict[Int, Match]

  val edgeSearch = for {
    e1 <- 0 until 4
    e2 <- 0 until 8
  } yield (e1, e2)

  def findMatches(tiles: Array[Tile]): Matches = {
    var result = m.MultiDict.empty[Int, Match]
    for {
      i <- 0 until tiles.size
      t1 = tiles(i)
      j <- i + 1 until tiles.size
      t2 = tiles(j)
      (e1, e2) <- edgeSearch.find({ case (e1, e2) => t1.edges(e1) == t2.edges(e2) })
    } {
      result += t1.id -> Match(t1.id, t2.id, e1)
      result += t2.id -> Match(t2.id, t1.id, e2 % 4)
    }
    result
  }

  def part1(tiles: Input): Long =
    val matches = findMatches(tiles)
    val corners = matches.filterSets(_._2.size == 2).keySet.map(_.toLong)
    corners.product

  def transformer[T: ClassTag](size: Int, offset: Int, source: Array[Array[T]]) = {
    val forward  = offset until (offset + size)
    val backward = forward.reverse

    (rotation: Rotation, flipped: Boolean) => {
      val result = Array.ofDim[T](size, size)
      val (iRange, jRange, rowOriented) = {
        if (!flipped) {
          rotation match {
            case Rotation.R0   => (forward, forward, true)
            case Rotation.R90  => (backward, forward, false)
            case Rotation.R180 => (backward, backward, true)
            case Rotation.R270 => (forward, backward, false)
          }
        } else {
          rotation match {
            case Rotation.R0   => (backward, backward, false)
            case Rotation.R90  => (backward, forward, true)
            case Rotation.R180 => (forward, forward, false)
            case Rotation.R270 => (forward, backward, true)
          }
        }
      }
      if (rowOriented) {
        var x = 0
        for { j <- jRange; i <- iRange } {
          result(x / size)(x % size) = source(j)(i)
          x += 1
        }
      } else {
        var x = 0
        for { i <- iRange; j <- jRange } {
          result(x / size)(x % size) = source(j)(i)
          x += 1
        }
      }
      result
    }
  }

  case class TilePlacement(tile: Tile, rotation: Rotation, flipped: Boolean) {
    def getEdge(i: Int): Int = tile.getEdge(i, rotation, flipped)

    def getImage: Array[Array[Char]] = {
      val cs  = tile.lines.map(_.toCharArray)
      val trx = transformer[Char](8, 1, cs)
      trx(rotation, flipped)
    }
  }

  def assembleTiles(tileMap: Map[Int, Tile], matches: Matches, gridSize: Int): Array[Array[TilePlacement]] = {
    val result = Array.ofDim[TilePlacement](gridSize, gridSize)

    // set top-left corner
    result(0)(0) = {
      val c0 = tileMap(matches.filterSets(_._2.size == 2).keySet.head)
      val r0 = {
        val es0 = matches.get(c0.id).map(_.edge1)
        if (es0 == Set(1, 2)) Rotation.R0
        else if (es0 == Set(2, 3)) Rotation.R90
        else if (es0 == Set(3, 0)) Rotation.R180
        else if (es0 == Set(0, 1)) Rotation.R270
        else throw new Exception("Invalid corner.")
      }
      TilePlacement(c0, r0, false)
    }

    for {
      j <- 0 until gridSize
      i <- 0 until gridSize
      if !(i == 0 && j == 0) // skip 0,0!
    } {
      val (prev, e1, e2) = {
        if (i == 0) (result(j - 1)(i), 2, 0)
        else (result(j)(i - 1), 1, 3)
      }
      val edge1 = inverse(prev.getEdge(e1))
      val nexts = for {
        id <- matches.get(prev.tile.id).map(_.tile2)
        tile = tileMap(id)
        r <- Rotation.all
        f <- Flip.all
        if edge1 == tile.getEdge(e2, r, f)
      } yield TilePlacement(tile, r, f)
      if (nexts.size != 1) {
        throw new Exception(s"Invalid matches: size ${nexts.size}")
      }
      result(j)(i) = nexts.head
    }

    result
  }

  def getImage(assembled: Array[Array[TilePlacement]], gridSize: Int): Array[Array[Char]] =
    val image = Array.ofDim[Char](gridSize * 8, gridSize * 8)
    for {
      j <- 0 until gridSize
      i <- 0 until gridSize
      tileImage = assembled(j)(i).getImage
      dj <- 0 until 8
      di <- 0 until 8
    } {
      image(j * 8 + dj)(i * 8 + di) = tileImage(dj)(di)
    }
    image

  //  |01234567890123456789
  // 0|                  #
  // 1|#    ##    ##    ###
  // 2| #  #  #  #  #  #
  val monster = Seq(
    (0, 18),
    (1, 0),
    (1, 5),
    (1, 6),
    (1, 11),
    (1, 12),
    (1, 17),
    (1, 18),
    (1, 19),
    (2, 1),
    (2, 4),
    (2, 7),
    (2, 10),
    (2, 13),
    (2, 16)
  )

  def findMonsters(img: Array[Array[Char]], imgSize: Int): Seq[(Int, Int)] =
    for {
      j <- 0 until (imgSize - 3)
      i <- 0 until (imgSize - 19)
      if monster.forall { case (dj, di) =>
        img(j + dj)(i + di) == '#'
      }
    } yield (j, i)

  def removeMonsters(img: Array[Array[Char]], imgSize: Int, ms: Seq[(Int, Int)]): Unit =
    for {
      (j, i)   <- ms
      (dj, di) <- monster
    } {
      img(j + dj)(i + di) = '.'
    }

  def part2(tiles: Input): Long =
    val gridSize  = math.sqrt(tiles.size).toInt
    val matches   = findMatches(tiles)
    val tileMap   = Map.from(tiles.map(t => t.id -> t))
    val assembled = assembleTiles(tileMap, matches, gridSize)

    var image     = getImage(assembled, gridSize)
    val size      = image(0).size
    val transform = transformer(size, 0, image)

    val (trxImage, ms) = (for {
      r <- Rotation.all
      f <- Flip.all
      trxImage = transform(r, f)
      ms       = findMonsters(trxImage, size)
      if ms.size > 0
    } yield (trxImage, ms)).head

    removeMonsters(trxImage, size, ms)
    trxImage.map(_.count(_ == '#')).sum
  end part2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
