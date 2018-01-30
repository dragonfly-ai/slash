package ai.dragonfly.math.stats

class StreamingStats {

  private var minObservation = Double.MaxValue
  private var maxObservation = Double.MinValue

  private var s0 = 0.0
  private var s1 = 0.0
  private var s2 = 0.0

  def apply(observation: Double, frequency: Double = 1.0): Unit = {
    maxObservation = Math.max(observation, maxObservation)
    minObservation = Math.min(observation, minObservation)

    s0 = s0 + frequency
    s1 = s1 + observation * frequency
    s2 = s2 + observation * observation * frequency
  }

  def min = minObservation
  def max = maxObservation
  def average = s1 / s0

  def standardDeviation: Option[Double] = {
    if (s0 < 2) None
    else Some(Math.sqrt(s0 * s2 - s1 * s1) / (s0 * (s0 - 1.0)))
  }

  override def toString(): String = s"Min: $min Max: $max Avg: $average STDV: $standardDeviation"

}
