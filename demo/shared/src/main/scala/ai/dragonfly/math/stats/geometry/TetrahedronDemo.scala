package ai.dragonfly.math.stats.geometry

import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.interval.*
import Interval.*
import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.vector.*

object TetrahedronDemo extends Demonstration {

  val `1/6`:Double = 1.0 / 6.0

  override def demo():Unit = {
    val q = 10.0 * Math.cos((Math.PI * 2.0)/3.0)

    val t = Tetrahedron(
      Vector3( q, q, q),
      Vector3(-q, q, q),
      Vector3( 0,-q, q),
      Vector3( 0, 0,-q)
    )

    val sampleCount:Int = 100

    println("# dragonfly.ai tetrahedronSamples.obj\n")
    println("o tetrahedronSamples\n")
    val samples = (0 until sampleCount).map {
      i =>
      val s = t.random()
      println(s"v ${s.x} ${s.y} ${s.z}\n")
      s
    }

//
//    def drawBoundedSamples(chart:Chart, bounds:Array[Vector2], samples:IndexedSeq[Vector2]):Chart = {
//      var v0:Vector2 = bounds.head
//      var tail = bounds.tail
//      while (tail.nonEmpty) {
//        for (v1 <- tail) chart.lineSegment(v0, v1, "Bounds")
//        v0 = tail.head
//        tail = tail.tail
//      }
//      chart.lineSegment(bounds.head, bounds.last, "Bounds")
//        .scatter("Samples", samples:_*)
//    }
//
//    val (chartWidth:Int, chartHeight:Int) = (99, 77)
//    val pad:Double = Math.abs(q)*0.15
//    val interval = `[]`[Double](Math.floor(Math.min(q, -q) - pad), Math.ceil(Math.max(q, -q) + pad))
//
//    println(
//      drawBoundedSamples(
//        Chart("Tetrahedron XY Plot", "x", "y", interval, interval, chartWidth, chartHeight),
//        t.vertices.map(v => Vector2(v.x, v.y)),
//        samples.map(v => Vector2(v.x, v.y))
//      )
//    ).append("\n")
//
//    println(
//      drawBoundedSamples(
//        Chart("Tetrahedron XZ Plot", "x", "z", interval, interval, chartWidth, chartHeight),
//        t.vertices.map(v => Vector2(v.x, v.z)),
//        samples.map(v => Vector2(v.x, v.z))
//      )
//    ).append("\n")
//
//    println(
//      drawBoundedSamples(
//        Chart("Tetrahedron YZ Plot", "y", "z", interval, interval, chartWidth, chartHeight),
//        t.vertices.map(v => Vector2(v.y, v.z)),
//        samples.map(v => Vector2(v.y, v.z))
//      )
//    ).append("\n")

  }

  override def name: String = "Tetrahedron"

}
