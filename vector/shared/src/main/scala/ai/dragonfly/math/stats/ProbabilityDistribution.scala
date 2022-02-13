package ai.dragonfly.math.stats

trait ProbabilityDistribution extends Sampleable[Double] {
  def μ:Double
  def mean: Double = μ

  def `σ²`:Double
  def variance: Double = `σ²`

  def σ: Double
  def standardDeviation: Double = σ

  def p(x:Double): Double

  def min:Double = Double.MinValue
  def MAX:Double = Double.MaxValue
}
