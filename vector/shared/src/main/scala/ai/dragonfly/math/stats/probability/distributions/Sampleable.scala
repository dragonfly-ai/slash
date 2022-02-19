package ai.dragonfly.math.stats.probability.distributions

trait Sampleable[DOMAIN] {
  def random(): DOMAIN
}
