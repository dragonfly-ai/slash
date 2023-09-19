package ai.dragonfly.math.matrix

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.matrix.util.*
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.vector.Vec.*
import narr.*

import ai.dragonfly.math.Constant.*

object DemoVectorInterop extends Demonstration {
  def demo(): Unit = {
    val M0: Matrix[2, 3] = Matrix[2, 3](
      NArray[NArray[Double]](
        NArray[Double](1.0, 1.46557123187676802665, `√2`),
        NArray[Double](2.0, π, e),
      )
    )

    val v2: Vec[2] = Vec[2](1.0, 2.0)

    println(s"${v2.show}.times($M0\n) = ${v2.times[3](M0)}")
  }

  def name: String = "DemoVectorInterop"
}
