package ai.dragonfly.math.stats.probability.distributions

trait Sampleable[T] {
  def random(): T
}
