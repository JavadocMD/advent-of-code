package aoc2022

import scala.annotation.tailrec

import aoc.Day

/* This solution realizes that the calculations in the two parts can be decomposed into four passes,
 * one for each direction, and then recombined. And by rotating the grid between each pass, we get
 * the result using the same code for each direction.
 */
object Day08b extends Day:
  type Grid = Vector[Vector[Int]]

  object Grid:
    val rotations = Seq[Grid => Grid](
      g => g,
      g => g.map(_.reverse),
      g => g.transpose,
      g => g.map(_.reverse)
    )
  end Grid

  extension (xs: Vector[Int]) def incremented(index: Int): Vector[Int] = xs.updated(index, xs(index) + 1)

  type Input = Grid
  def parse(xs: Iterator[String]): Input = xs.map(_.map(_.toString.toInt).toVector).toVector

  def part1(input: Input) =
    val size = input.size // input is square

    // which trees are visible from the "left"?
    // if visible, increment a tree's visibility score by 1
    def rowVisibility(trees: Vector[Int], prevVis: Vector[Int]): Vector[Int] =
      // sweep is the tallest tree we've seen so far
      @tailrec def recurse(i: Int, sweep: Int, vis: Vector[Int]): Vector[Int] =
        if i >= size then vis
        else if sweep < trees(i) then recurse(i + 1, trees(i), vis.incremented(i))
        else recurse(i + 1, sweep, vis)
      recurse(0, -1, prevVis)

    // combine unidirectional visibility
    def combineVisibility(tree: Grid, prevVis: Grid): Grid =
      (tree zip prevVis).map { rowVisibility(_, _) }

    // and we get total visibility by rotating four times
    val z: (Grid, Grid) = (input, Vector.fill(size, size)(0))
    val (_, vis) = Grid.rotations.foldLeft(z) { case ((trees, vis), rotate) =>
      val t = rotate(trees)
      val v = rotate(vis)
      (t, combineVisibility(t, v))
    }

    // then count visible trees
    vis.map(_.count(_ > 0)).sum
  end part1

  def part2(input: Input) =
    val size = input.size // input is square

    // how many trees can I see from `from` until the end of this row?
    // this is unidirectional visibility
    def countTrees(row: Vector[Int], from: Int): Int =
      @tailrec def recurse(i: Int, visible: Int): Int =
        if i >= size then visible
        else if row(i) >= row(from) then visible + 1
        else recurse(i + 1, visible + 1)
      recurse(from + 1, 0)

    // combine unidirectional scenic score
    def combineScore(tree: Grid, prevScore: Grid): Grid =
      Vector.tabulate(size, size) { (j, i) =>
        prevScore(j)(i) * countTrees(tree(j), i)
      }

    // and we get the total scores by rotating four times
    val z: (Grid, Grid) = (input, Vector.fill(size, size)(1))
    val (_, score) = Grid.rotations.foldLeft(z) { case ((trees, score), rotate) =>
      val t = rotate(trees)
      val s = rotate(score)
      (t, combineScore(t, s))
    }

    // then find the best tree
    score.map(_.max).max
  end part2

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().iterator)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
