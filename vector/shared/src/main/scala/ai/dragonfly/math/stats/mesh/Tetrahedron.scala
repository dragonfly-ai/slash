package ai.dragonfly.math.stats.mesh

import ai.dragonfly.math.stats.Sampleable3
import ai.dragonfly.math.vector.Vector3

case class Tetrahedron(v1: Vector3, v2: Vector3, v3: Vector3, v4: Vector3 = Vector3(0.0, 0.0, 0.0)) extends Sampleable3 {

  private val v1_ = v1.copy().subtract(v4)
  private val v2_ = v2.copy().subtract(v4)
  private val v3_ = v3.copy().subtract(v4)

  override def draw(): Vector3 = {

    var w1 = Math.random()
    var w2 = Math.random()
    var w3 = Math.random()

    if (w1 > w2) {
      val t = w1
      w1 = w2
      w2 = t
    }

    if (w2 > w3) {
      val t = w2
      w2 = w3
      w3 = t
    }

    if (w1 > w2) {
      val t = w1
      w1 = w2
      w2 = t
    }

    v1_.copy().scale(w1)
      .add(v2_.copy().scale(w2 - w1))
      .add(v3_.copy().scale(w3 - w2)).add(v4)

  }
}
