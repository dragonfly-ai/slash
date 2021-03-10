package ai.dragonfly.math.stats

import ai.dragonfly.math.vector.Vector3

import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
trait Sampleable[T] {
  def random():T
}
