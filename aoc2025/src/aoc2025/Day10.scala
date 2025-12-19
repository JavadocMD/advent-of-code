package aoc2025

import scala.annotation.tailrec
import aoc.Day
import com.google.ortools.Loader
import com.google.ortools.linearsolver.{MPConstraint, MPObjective, MPSolver, MPVariable}
import com.google.ortools.linearsolver.MPSolver.ResultStatus

object Day10 extends Day:
  Loader.loadNativeLibraries() // initialize OR-tools

  type Input = List[Machine]

  type Lights   = Vector[Boolean]
  type Joltages = Vector[Int]
  type Button   = List[Int]

  case class Machine(config: Lights, buttons: List[Button], joltages: Joltages):
    def initialLights = Vector.fill(this.config.size)(false)
    def pressLights(state: Lights, button: Button): Lights =
      button.foldLeft(state): (acc, i) =>
        acc.updated(i, !acc(i))
    def isDoneLights(state: Lights) = state == this.config
  end Machine

  def parse(xs: List[String]): Input = xs.map:
    case s"[$lights] $buttons {$joltages}" =>
      val cnfg = lights.toVector.map(_ == '#')
      val btns = buttons
        .split("\\s")
        .toList
        .map:
          case s"($nums)" => nums.split(",").toList.map(_.toInt)
      val jtgs = joltages.split(",").toVector.map(_.toInt)
      Machine(cnfg, btns, jtgs)

  def shortestPath1(machine: Machine): Int =
    @tailrec
    def recurse(open: Vector[Lights], closed: Map[Lights, Int]): Int =
      val curr   = open.head
      val length = closed(curr)

      if machine.isDoneLights(curr) then length
      else
        val next = machine.buttons
          .map: b =>
            machine.pressLights(curr, b)
          .filter: s =>
            length + 1 < closed(s)
          .toVector
        recurse(open.tail ++ next, closed ++ next.map(s => s -> (length + 1)))

    recurse(
      Vector(machine.initialLights),
      Map(machine.initialLights -> 0).withDefaultValue(Int.MaxValue),
    )

  def part1(input: Input) = input.map(shortestPath1).sum

  extension (value: Double) def isInteger: Boolean = (value - scala.math.round(value)).abs < 0.000001

  def solve(machine: Machine, geq: Double = 0.0): Long =
    // Integer Linear Programming Time! Brought to you by OR-tools.
    val solver = MPSolver.createSolver("SCIP")

    // Each button gets a variable (the number of times we press it).
    // e.g.: b0, ..., bn
    val btnVars =
      for bIndex <- 0 to machine.buttons.size
      yield solver.makeIntVar(0.0, Double.PositiveInfinity, s"b$bIndex")

    // Multiple buttons have a relationship to a single joltage value.
    // These are captured as equality constraints.
    // e.g.: [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
    //   joltage 0:                     b4 + b5 = 3
    //   joltage 1:      b1 +                b5 = 5
    //   joltage 2:           b2 + b3 + b4      = 4
    //   joltage 3: b0 + b1 +      b3           = 7
    val buttonJoltage =
      for (joltage, jIndex) <- machine.joltages.zipWithIndex
      yield
        val cns = solver.makeConstraint(joltage, joltage, s"j$jIndex")
        for
          (button, bvar) <- machine.buttons.zip(btnVars)
          coeff = if button.contains(jIndex) then 1.0 else 0.0
        do cns.setCoefficient(bvar, coeff)

    // Minimize the objective: total times a button is pressed.
    // e.g.:        b0 + b1 + b2 + b3 + b4 + b5 = obj
    val objective = solver.objective()
    for bvar <- btnVars do objective.setCoefficient(bvar, 1.0)
    objective.setMinimization()

    val result = solver.solve()

    // Sanity checks.
    if result != ResultStatus.OPTIMAL then throw new Exception("non-optimal solution")
    if !btnVars.forall(_.solutionValue().isInteger) then throw new Exception("non-integer solution")
    if !btnVars.forall(_.solutionValue() >= 0.0) then throw new Exception("less-than-zero solution")

    objective.value().toLong

  def part2(input: Input) = input.map(solve(_)).sum

  final def main(args: Array[String]): Unit =
    val in = parse(loadInput().toList)
    solveP1(() => part1(in))
    solveP2(() => part2(in))
