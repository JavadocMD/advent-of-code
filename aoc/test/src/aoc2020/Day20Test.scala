package aoc2020

import utest._

object Day20Test extends TestSuite:
  import Day20._

  val input = load("aoc2020/Day20e.input.txt")
  val tiles = toChunks(input).map(Tile.from)

  val tests = Tests {
    "find matches" - {
      val matches = findMatches(tiles)
      matches.keySet.size ==> 9
      matches.toList.size ==> 24
    }

    "do part 1" - {
      part1(tiles) ==> 20899048083289L
    }

    "assemble tiles" - {
      val matches   = findMatches(tiles)
      val tileMap   = Map.from(tiles.map(t => t.id -> t))
      val assembled = assembleTiles(tileMap, matches, 3)

      val tileIds = assembled.map(_.map(_.tile.id))
      val trx     = transformer(3, 0, tileIds)

      // At least there should be *some* orientation which matches.
      // Which orientation might change if internals change.
      val res = trx(Rotation.R0, true)

      res(0)(0) ==> 1951
      res(0)(1) ==> 2311
      res(0)(2) ==> 3079

      res(1)(0) ==> 2729
      res(1)(1) ==> 1427
      res(1)(2) ==> 2473

      res(2)(0) ==> 2971
      res(2)(1) ==> 1489
      res(2)(2) ==> 1171
    }

    "assemble image" - {
      val matches   = findMatches(tiles)
      val tileMap   = Map.from(tiles.map(t => t.id -> t))
      val assembled = assembleTiles(tileMap, matches, 3)

      val image = getImage(assembled, 3)
      val size  = 24
      val trx   = transformer(size, 0, image)

      val expected = """
        |.#.#..#.##...#.##..#####
        |###....#.#....#..#......
        |##.##.###.#.#..######...
        |###.#####...#.#####.#..#
        |##.#....#.##.####...#.##
        |...########.#....#####.#
        |....#..#...##..#.#.###..
        |.####...#..#.....#......
        |#..#.##..#..###.#.##....
        |#.####..#.####.#.#.###..
        |###.#.#...#.######.#..##
        |#.####....##..########.#
        |##..##.#...#...#.#.#.#..
        |...#..#..#.#.##..###.###
        |.#.#....#.##.#...###.##.
        |###.#...#..#.##.######..
        |.#.#.###.##.##.#..#.##..
        |.####.###.#...###.#..#.#
        |..#.#..#..#.#.#.####.###
        |#..####...#.#.#.###.###.
        |#####..#####...###....##
        |#.##..#..#...#..####...#
        |.#.###..##..##..####.##.
        |...###...##...#...#..###""".stripMargin.trim // .linesIterator.toArray.map(_.toCharArray)

      // At least there should be *some* orientation which matches.
      // Which orientation might change if internals change.
      val imageString = trx(Rotation.R0, true).map(_.mkString).mkString("\n")
      imageString ==> expected
    }

    "do part 2" - {
      part2(tiles) ==> 273
    }
  }
