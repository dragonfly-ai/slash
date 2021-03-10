package ai.dragonfly.math.stats

import ai.dragonfly.math.util.Demonstrable

import scala.scalajs.js.annotation.JSExportAll
import scala.util.Random

@JSExportAll
object Gaussian extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val gaussian:Gaussian = Gaussian(10.0, 6.5)
    val histogram:DenseDiscreteHistogram = new DenseDiscreteHistogram(60, -20.0, 40.0)
    for(i <- 0 until 1000){
      histogram(gaussian.random())
    }
    sb.append(histogram)
  }

  override def name: String = "Gaussian"
}

@JSExportAll
case class Gaussian(average:Double, standardDeviation:Double) extends Sampleable[Double] {
  override def random(): Double = average + ( Random.nextGaussian() * standardDeviation )
}
