package ai.dragonfly.math.stats.geometry

import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.examples.Demonstrable
import ai.dragonfly.math.vector.Vector3

object Tetrahedron extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val t = Tetrahedron(
      Vector3(1, 1, 1),
      Vector3(2, 1, 1),
      Vector3(1, 2, 1),
      Vector3(1, 1, 2)
    )

    sb.append("# dragonfly.ai ''")
    sb.append("o tetrahedronSamples")
    for (i <- 0 until 25) {
      val s = t.random()
      sb.append(s"v ${s.x} ${s.y} ${s.z}\n")
    }
    sb
  }

  override def name: String = "Tetrahedron"
}

case class Tetrahedron(v1: Vector3, v2: Vector3, v3: Vector3, v4: Vector3 = Vector3(0.0, 0.0, 0.0)) extends Sampleable[Vector3] {

  private val v1_ = v1.copy().subtract(v4)
  private val v2_ = v2.copy().subtract(v4)
  private val v3_ = v3.copy().subtract(v4)

  // Formula for the Volume of a tetrahedron taken from:
  // https://n-e-r-v-o-u-s.com/blog/?p=4415
  def volume: Double = v1_.cross(v2).dot(v3) / 6.0

  override def random(): Vector3 = {

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
