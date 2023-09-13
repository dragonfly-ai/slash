package ai.dragonfly.math.matrix

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.matrix.decomposition.*
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.vector.Vec.*
import narr.*

object DemoEigenDecomposition extends Demonstration {
  def demo(): Unit = {
    val M0: Matrix[4, 4] = Matrix[4, 4](
      NArray[NArray[Double]](
        NArray[Double](1.0, 2.0, 3.0, 4.0),
        NArray[Double](0.0, -1.0, 0.0, -3.0),
        NArray[Double](4.0, 0.0, -7.0, 0.5),
        NArray[Double](0.27, ai.dragonfly.math.Constant.π, 1.1, 0.5),
      )
    )

    println(M0)
    println("\n\n")
    val eig:Eigen[4] = Eigen[4](M0)
    println(eig.realEigenvalues.render())
    println("\n\n")

  }

  def name: String = "DemoEigenValueDecomposition"
}
