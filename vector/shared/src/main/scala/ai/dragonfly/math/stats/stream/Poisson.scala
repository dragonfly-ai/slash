package ai.dragonfly.math.stats.stream

import ai.dragonfly.math.stats.Sampleable
import ai.dragonfly.math.util.Demonstrable

import scala.language.postfixOps
import scala.language.implicitConversions

object Poisson extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val idealPoisson:ai.dragonfly.math.stats.Poisson = ai.dragonfly.math.stats.Poisson(20)
    sb.append(s"Populate stream.Poisson by sampling from: $idealPoisson")
    val p1:Poisson = new Poisson
    for (i <- 0 until 10000) {
      p1(idealPoisson.random())
    }
    sb.append(s"$p1")
  }

  override def name: String = "stream.Poisson"
}

class Poisson extends Sampleable[Int] {
  private var minObservation = Double.MaxValue
  private var maxObservation:Long = Long.MinValue

  private var s0 = 0.0 // weighted count
  private var s1 = 0.0 // weighted sum

  /**
   * Assumes only positive valued observations.
   * @param observation the value observed.
   * @param frequency the number of times this value has been observed, default 1L
   */
  def apply(observation: Int, frequency: Int = 1):Poisson = if (observation < 0) {
    throw PoissonDistributionUndefinedForNegativeNumbers(observation)
  } else {
    minObservation = Math.min(observation, minObservation)
    maxObservation = Math.max(observation, maxObservation)

    s0 = s0 + frequency
    s1 = s1 + observation * frequency

    this
  }

  def min:Double = minObservation
  def max:Long = maxObservation

  def sampleSize:Double = s0

  @inline def mean:Double = s1 / s0 // λ

  @inline def variance: Double = s1 / s0 // also λ

  def standardDeviation:Double = Math.sqrt(variance)

  override def toString: String = s"stream.Poisson(min = $min, MAX = $max, λ = $mean, √λ = $standardDeviation, n = $s0)"

//  /**
//   * Approximate probability of x, given this Poisson distribution.
//   * @param x a value in the probability distribution
//   * @return P(x)
//   */
//  def P(x:Int):Double = {
//    if (x > max || x < 0) return 0 // ugly hack!
//    val scalar:Double = 100.0 / max
//    val scaledX:Int = Math.round(x*scalar).toInt
//    val lambda:Double = average * scalar
//    (BigDecimal(Math.pow(Math.E, -lambda) * Math.pow(lambda, scaledX)) / (scaledX!) ).toDouble
//  }

  /**
   * Generate a random variable from this Poisson Distribution.
   * @return
   */
  override def random(): Int = ai.dragonfly.math.stats.Poisson(mean).random()

    /*
    val scalar:Double = 100.0 / max
    val `λ`:Double = average * scalar
    val `e^-λ` = Math.pow(Math.E, -`λ`)

    val probability:Array[BigDecimal] = new Array[BigDecimal](101)
    var total:BigDecimal = BigDecimal( `e^-λ` * Math.pow(`λ`, 0) ) / (0!)
    var lambdaPower:Double = Math.pow(`λ`, 0)
    probability(0) = total
    for (i <- 1 to 100) {
      lambdaPower = lambdaPower * `λ`
      val term:BigDecimal = BigDecimal( `e^-λ` * lambdaPower ) / (i!)
      total = total + term
      probability(i) = term
    }

    val cumulative:Array[BigDecimal] = new Array[BigDecimal](101)
    cumulative(0) = probability(0)
    for (i <- 1 to 100) {
      cumulative(i) = cumulative(i-1) + probability(i)
    }

    val seed:BigDecimal = total * Random.nextDouble()
    var i = 1
    while(cumulative(i) < seed) {
      i = i + 1
    }
    ((i - 1)*scalar).toInt
  }
 */
}

case class PoissonDistributionUndefinedForNegativeNumbers(negative:Int) extends Exception(s"Poisson distribution undefined for observation: $negative")