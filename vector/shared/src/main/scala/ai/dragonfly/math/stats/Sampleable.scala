package ai.dragonfly.math.stats

import ai.dragonfly.math.vector.Vector3

trait Sampleable[T] {
  def random():T
}
