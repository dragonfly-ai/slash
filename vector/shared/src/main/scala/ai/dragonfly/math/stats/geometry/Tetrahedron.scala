package ai.dragonfly.math.stats.geometry

import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.example.Demonstrable
import ai.dragonfly.math.interval.*
import Interval.*
import ai.dragonfly.math.vector.Vector3

object Tetrahedron extends Demonstrable {

  val `1/6`:Double = 1.0 / 6.0

  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val q = 10.0 * Math.cos((Math.PI * 2.0)/3.0)

    val t = Tetrahedron(
      Vector3( q, q, q),
      Vector3(-q, q, q),
      Vector3( 0,-q, q),
      Vector3( 0, 0,-q)
    )

    val sampleCount:Int = 100

    sb.append("# dragonfly.ai tetrahedronSamples.obj\n")
    sb.append("o tetrahedronSamples\n")
    val samples = (0 until sampleCount).map {
      i =>
      val s = t.random()
      sb.append(s"v ${s.x} ${s.y} ${s.z}\n")
      s
    }

    import ai.dragonfly.math.visualization.Chart
    import ai.dragonfly.math.vector.Vector2

    def drawBoundedSamples(chart:Chart, bounds:Array[Vector2], samples:IndexedSeq[Vector2]):Chart = {
      var v0:Vector2 = bounds.head
      var tail = bounds.tail
      while (tail.nonEmpty) {
        for (v1 <- tail) chart.lineSegment(v0, v1, "Bounds")
        v0 = tail.head
        tail = tail.tail
      }
      chart.lineSegment(bounds.head, bounds.last, "Bounds")
        .scatter("Samples", samples:_*)
    }

    val (chartWidth:Int, chartHeight:Int) = (99, 77)
    val pad:Double = Math.abs(q)*0.15
    val interval = `[]`[Double](Math.floor(Math.min(q, -q) - pad), Math.ceil(Math.max(q, -q) + pad))

    sb.append(
      drawBoundedSamples(
        Chart("Tetrahedron XY Plot", "x", "y", interval, interval, chartWidth, chartHeight),
        t.vertices.map(v => Vector2(v.x, v.y)),
        samples.map(v => Vector2(v.x, v.y))
      )
    ).append("\n")

    sb.append(
      drawBoundedSamples(
        Chart("Tetrahedron XZ Plot", "x", "z", interval, interval, chartWidth, chartHeight),
        t.vertices.map(v => Vector2(v.x, v.z)),
        samples.map(v => Vector2(v.x, v.z))
      )
    ).append("\n")

    sb.append(
      drawBoundedSamples(
        Chart("Tetrahedron YZ Plot", "y", "z", interval, interval, chartWidth, chartHeight),
        t.vertices.map(v => Vector2(v.y, v.z)),
        samples.map(v => Vector2(v.y, v.z))
      )
    ).append("\n")

    sb
  }

  override def name: String = "Tetrahedron"

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
