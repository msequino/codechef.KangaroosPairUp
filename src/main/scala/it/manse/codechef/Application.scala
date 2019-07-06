package it.manse.codechef

object Application extends App {

  override def main(args: Array[String]) : Unit = {
    val M = 12
    val pairs = for { a <- 1 to M; b <- 1 to a if stepSameFootStep(a, b, M) && arriveAtEnd(a, b, M)} yield (a, b)

    pairs.foreach { case (a, b) => println(s"($a, $b)") }
  }
  private def stepSameFootStep(a: Int, b: Int, M: Int) =  !(for( x <- 1 until M / a; y <- 1 until M / b ) yield {
    if (a * x == b * y) true else false
  }).exists( atLeastOneTrue => atLeastOneTrue)
  private def arriveAtEnd(a: Int, b: Int, M: Int) = M % b == 0 && M % a == 0
}
