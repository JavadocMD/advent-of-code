import mill._
import mill.scalalib._

object aoc extends ScalaModule {
  def scalaVersion = "3.3.3"

  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parallel-collections:1.0.4",
    ivy"org.scala-lang.modules::scala-collection-contrib:0.3.0",
    ivy"org.apache.commons:commons-math3:3.6.1",
    ivy"com.lihaoyi::fastparse:3.1.0"
  )

  object test extends ScalaTests {
    def ivyDeps       = Agg(ivy"com.lihaoyi::utest:0.8.3")
    def testFramework = "utest.runner.Framework"
  }
}
