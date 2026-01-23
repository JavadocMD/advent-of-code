import mill._
import mill.scalalib._

trait AocYearModule extends ScalaModule {
  def scalaVersion = "3.3.7"
  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parallel-collections:1.0.4",
    ivy"org.scala-lang.modules::scala-collection-contrib:0.3.0",
    ivy"org.apache.commons:commons-math3:3.6.1",
    ivy"com.google.ortools:ortools-java:9.14.6206",
    ivy"com.lihaoyi::fastparse:3.1.0",
    ivy"com.lihaoyi::upickle:4.4.1",
  )
  object test extends ScalaTests {
    def ivyDeps       = Agg(ivy"com.lihaoyi::utest:0.8.3")
    def testFramework = "utest.runner.Framework"
  }
}

object aoc2015 extends AocYearModule
object aoc2016 extends AocYearModule
object aoc2019 extends AocYearModule
object aoc2020 extends AocYearModule
object aoc2021 extends AocYearModule
object aoc2022 extends AocYearModule
object aoc2023 extends AocYearModule
object aoc2024 extends AocYearModule
object aoc2025 extends AocYearModule
