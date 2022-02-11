package ai.dragonfly.math.stats

trait ProbabilityDistribution extends Sampleable[Double] {
  def mean: Double
  def variance: Double
  def standardDeviation: Double
  def p(x:Double): Double
}
