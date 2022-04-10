package ai.dragonfly.math.stats.probability.distributions


trait Sampleable[DOMAIN] {
  def random(r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): DOMAIN
}
