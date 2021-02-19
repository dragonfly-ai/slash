package ai.dragonfly.math.vector.native

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import ai.dragonfly.math.vector.{Vector => V3CT0R}

trait Vector {
  val handle:V3CT0R = this.asInstanceOf[V3CT0R]
  @JSExport("values") def jsValues: js.Array[Double] = {
    val vs = handle.values
    val out = js.Array[Double](vs.length)
    for (i <- vs.indices) out(i) = vs(i)
    out
  }
  @JSExport def center(vectors: js.Array[V3CT0R]): js.Array[V3CT0R] = {
    for (v: V3CT0R <- vectors) v.subtract(handle)
    vectors
  }

  @JSExport def average(vectors: js.Array[V3CT0R]): js.Array[V3CT0R] = {
    for (v: V3CT0R <- vectors) v.subtract(handle)
    vectors
  }


}
