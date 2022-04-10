package ai.dragonfly.math.stats

import ai.dragonfly.math.interval.Interval

object BoundedMean {
  def apply[DOMAIN: Numeric](μ:Double, bounds: Interval[DOMAIN], ℕ:DOMAIN):BoundedMean[DOMAIN] = {
    if (bounds.rangeContains(μ)) new BoundedMean[DOMAIN](μ, bounds, ℕ)
    else throw MeanOutsideBounds[DOMAIN](μ, bounds)
  }
}

case class BoundedMean[DOMAIN: Numeric](μ:Double, bounds: Interval[DOMAIN], ℕ:DOMAIN) {
  def min:DOMAIN = bounds.min
  def MAX:DOMAIN = bounds.MAX
}

case class MeanOutsideBounds[DOMAIN](μ:Double, bounds: Interval[DOMAIN]) extends Exception(
  s"Cannot create BoundedMean(μ = $μ, bounds = $bounds).  $μ lies outside of $bounds."
)