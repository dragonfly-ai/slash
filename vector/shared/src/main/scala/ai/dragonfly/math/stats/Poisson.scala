package ai.dragonfly.math.stats

import ai.dragonfly.math.util.Demonstrable

import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
object Poisson extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val idealPoisson:Poisson = Poisson(15)
    val histogram:DenseDiscreteHistogram = new DenseDiscreteHistogram(60, 0.0, 70.0)
    for(i <- 0 until 1000){
      histogram(idealPoisson.random())
    }
    sb.append(histogram)
  }

  override def name: String = "Poisson"
}

@JSExportAll
case class Poisson(lambda:Double) extends Sampleable[Int] {
  def average:Double = lambda
  def variance:Double = lambda
  lazy val stdDev:Double = Math.sqrt(lambda)
  def standardDeviation:Double = stdDev

  // Knuth's method:
  override def random(): Int = {
    val e = Math.E
    val L = BigDecimal(Math.pow(e, -lambda))
    var k = 0
    var p = 1.0
    while (p > L) {
      k = k + 1
      p = p * Math.random()
    }
    k - 1
  }
}
