package aoc2019

import aoc._
import scala.annotation.tailrec

import aoc.Day

object Day24 extends Day:
  type Input = String
  def parse(xs: Array[String]): Input = xs.mkString

  // Note: I originally solved part 1 with Int, but part 2 clearly requires Long.
  // For some reason Scala's bitwise operators aren't on Integral or any
  // other typeclass I could find. So to avoid writing two versions of some of these
  // functions, I rewrote part 1 to use Long as well. Just as fast by my measure.

  def b(s: String): Long = java.lang.Long.parseLong(s, 2)

  /** Counts the number of bits that are set in `n`; Kernighan's method. */
  @tailrec
  final def setBits(n: Long, acc: Int = 0): Int =
    if (n == 0L) acc else setBits(n & (n - 1), acc + 1)

  // Define grid like this:
  //   A|B|C|D|E
  //   F|G|H|I|J
  //   K|L|M|N|O
  //   P|Q|R|S|T
  //   U|V|W|X|Y

  // In part 1, the level is represented as a bitmap of 25 bits:
  // bits 24-0 => Y,X,W,V,U,T,S,R,Q,P,O,N,M,L,K,J,I,H,G,F,E,D,C,B,A

  def parse1(s: String): Long = b(s.reverse.map(c => if (c == '#') '1' else '0'))

  // In part 2, each level cares about:
  // - 24 tiles on this level (note we drop M entirely)
  // - 4 tiles on the level above
  // - 16 tiles on the level below
  // 44 bits per level => Long
  //
  // So we pack them into a Long like this:
  // bits 43-40 => [level - 1; H,N,R,L]
  // bits 39-24 => [level + 1; A,B,C,D,E,J,O,T,Y,X,W,V,U,P,K,F]
  // bits 23-0  => [level    ; H,N,R,L|G,I,S,Q|A,B,C,D,E,J,O,T,Y,X,W,V,U,P,K,F]
  //
  // The least 24 bits is *this* level,
  // the level below cares about our bits 23-20
  // the level above cares about our bits 15-0
  val aboveMask = b("111100000000000000000000")
  val belowMask = b("000000001111111111111111")
  val levelMask = b("111111111111111111111111")

  def parse2(s: String): Long =
    val indices = Seq(7, 13, 17, 11, 6, 8, 18, 16, 0, 1, 2, 3, 4, 9, 14, 19, 24, 23, 22, 21, 20, 15, 10, 5)
    val cs      = for { i <- indices; c = s(i) } yield if (c == '#') '1' else '0'
    b(cs.mkString)

  /** Get the bits we care about from the level above and shift them for this level. */
  def bitsAbove(above: Long): Long = (above & aboveMask) << 20

  /** Get the bits we care about from the level below and shift them for this level. */
  def bitsBelow(below: Long): Long = (below & belowMask) << 24

  /** Bitmask for the neighbors of any index 0-24. */
  val neighborMask1 = for {
    y <- 0 until 5
    x <- 0 until 5
  } yield {
    val i    = y * 5 + x
    var mask = 0L
    if (x > 0) mask |= (1 << (i - 1))
    if (x < 4) mask |= (1 << (i + 1))
    if (y > 0) mask |= (1 << (i - 5))
    if (y < 4) mask |= (1 << (i + 5))
    mask
  }

  /** Bitmask for the neighbors for any index 0-23. */
  val neighborMask2 = {
    // Layer above masks:
    val Ha = 1L << 43
    val Na = 1L << 42
    val Ra = 1L << 41
    val La = 1L << 40
    // Layer below masks:
    //            ABCDEJOTYXWVUPKF
    val AEb = b("1111100000000000") << 24
    val EYb = b("0000111110000000") << 24
    val YUb = b("0000000011111000") << 24
    val UAb = b("1000000000001111") << 24
    // This layer masks:
    val H = 1L << 23
    val N = 1L << 22
    val R = 1L << 21
    val L = 1L << 20
    val G = 1L << 19
    val I = 1L << 18
    val S = 1L << 17
    val Q = 1L << 16
    val A = 1L << 15
    val B = 1L << 14
    val C = 1L << 13
    val D = 1L << 12
    val E = 1L << 11
    val J = 1L << 10
    val O = 1L << 9
    val T = 1L << 8
    val Y = 1L << 7
    val X = 1L << 6
    val W = 1L << 5
    val V = 1L << 4
    val U = 1L << 3
    val P = 1L << 2
    val K = 1L << 1
    val F = 1L
    // Neighbor mask for this level (bits 23-0):
    IndexedSeq(
      /*H*/ G | C | I | AEb,
      /*N*/ I | O | S | EYb,
      /*R*/ Q | S | W | YUb,
      /*L*/ G | K | Q | UAb,
      /*G*/ B | H | L | F,
      /*I*/ D | J | N | H,
      /*S*/ N | T | X | R,
      /*Q*/ L | R | V | P,
      /*A*/ B | F | Ha | La,
      /*B*/ A | C | G | Ha,
      /*C*/ B | D | H | Ha,
      /*D*/ C | E | I | Ha,
      /*E*/ D | J | Ha | Na,
      /*J*/ E | I | O | Na,
      /*O*/ J | N | T | Na,
      /*T*/ O | S | Y | Na,
      /*Y*/ T | X | Na | Ra,
      /*X*/ S | Y | W | Ra,
      /*W*/ R | X | V | Ra,
      /*V*/ Q | W | U | Ra,
      /*U*/ P | V | Ra | La,
      /*P*/ K | Q | U | La,
      /*K*/ F | L | P | La,
      /*F*/ A | G | K | La
    ).reverse
  }

  /** Evolves the bitmap of our current state using the given mask of neighbors, for the given number of bits. This came
    * about because: part 1 uses 25 bits per level (of course there's only one level), while part 2 uses 24 bits per
    * level and each has different neighbor masks.
    */
  def evolve(bits: Int, neighborMask: IndexedSeq[Long])(state: Long): Long =
    var result = 0
    for { i <- (bits - 1) to 0 by -1 } {
      // neighbors
      val n = setBits(state & neighborMask(i))
      // prev value
      val p = (state >>> i) & 1
      // next value
      val x = (p, n) match {
        case (1, 1) => 1 // live if exactly 1 neighbor
        case (0, 1) => 1 // become infested if 1 neighbor
        case (0, 2) => 1 // become infested if 2 neighbors
        case _      => 0 // otherwise die or don't become infested
      }
      result = (result | x) << 1
    }
    result >>> 1

  def part1(input: String): Long =
    // Just repeatedly evolve until we come across a value we've seen before.
    // A state's biodiversity is literally just its decimal value.
    val evolve1 = evolve(25, neighborMask1) _
    @tailrec
    def recurse(curr: Long, seen: Set[Long]): Long = {
      if (seen.contains(curr)) curr
      else recurse(evolve1(curr), seen + curr)
    }
    recurse(parse1(input).toLong, Set.empty)
  end part1

  def part2(input: String, iterations: Int = 200): Int =
    // Evolves all levels, then merges above/below.
    val evolve2 = evolve(24, neighborMask2) _
    def evolveAll2(state: IndexedSeq[Long]): IndexedSeq[Long] = {
      val evolved = state.map(evolve2)
      val n       = evolved.size
      for {
        i <- 0 until n
        above = if (i > 0) evolved(i - 1) else 0L
        below = if (i < n - 1) evolved(i + 1) else 0L
      } yield evolved(i) | bitsAbove(above) | bitsBelow(below)
    }

    // Just have to repeat this `iterations` times...
    var state = IndexedSeq(parse2(input))
    for { i <- 0 until iterations } {
      // expand if outer layers are non-zero
      if ((state.head & levelMask) != 0L) {
        state = state.prepended(bitsBelow(state.head))
      }
      if ((state.last & levelMask) != 0L) {
        state = state.appended(bitsAbove(state.last))
      }
      // then evolve
      state = evolveAll2(state)
    }

    // Then count the total number of bits (but only the ones unique to each level).
    state.map(x => setBits(x & levelMask)).sum
  end part2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput())
    solveP1(() => part1(in))
    solveP2(() => part2(in))
