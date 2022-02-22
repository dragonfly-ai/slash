package ai.dragonfly.math.stats

import ai.dragonfly.math.interval.Interval

case class PointStatistics[DOMAIN: Numeric](μ:Double, `σ²`:Double, bounds: Interval[DOMAIN], ℕ̂:DOMAIN) {
  def min:DOMAIN = bounds.min
  def MAX:DOMAIN = bounds.MAX
}
