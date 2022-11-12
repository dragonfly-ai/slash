package ai.dragonfly.math.stats.geometry

import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.interval.*
import Interval.*
import ai.dragonfly.math.vector.Vector3

object Tetrahedron {

  val `1/6`:Double = 1.0 / 6.0

  def apply(v1: Vector3, v2: Vector3, v3: Vector3, v4: Vector3):Tetrahedron = Tetrahedron(Array[Vector3](v1, v2, v3, v4))

}

case class Tetrahedron(vertices:Array[Vector3]) extends Sampleable[Vector3] {

  import Tetrahedron.*

  inline def v1:Vector3 = vertices(0)
  inline def v2:Vector3 = vertices(1)
  inline def v3:Vector3 = vertices(2)
  inline def v4:Vector3 = vertices(3)

  private def `v1-v4` = v1 - v4
  private def `v2-v4` = v2 - v4
  private def `v3-v4` = v3 - v4

  // Formula for the Volume of a tetrahedron taken from:
  // https://en.wikipedia.org/wiki/Tetrahedron#Volume

  def volume:Double = `1/6` * Math.abs(`v1-v4` dot (`v2-v4` ⨯ `v3-v4`))

  override def random(r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): Vector3 = {

    var w1 = r.nextDouble()
    var w2 = r.nextDouble()
    var w3 = r.nextDouble()

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

    (`v1-v4` * w1).add(`v2-v4` * (w2 - w1)).add(`v3-v4` * (w3 - w2)).add(v4)
  }


}
