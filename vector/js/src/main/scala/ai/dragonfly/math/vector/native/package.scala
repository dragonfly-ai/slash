package ai.dragonfly.math.vector

package object native {
  type VectorValues = scala.scalajs.js.Array[Double]
  type VECTORS = scala.scalajs.js.Array[Vector]

  //val VectorValues:VectorValuesObject = scala.scalajs.js.Array
  object VectorValues {
    export scala.scalajs.js.Array.*

    def fill(dimension: Int)(d: Double): ai.dragonfly.math.vector.VectorValues = {
      val values: VectorValues = new VectorValues(dimension)
      for (i <- values.indices) values(i) = d
      values
    }

    def tabulate(dimension: Int)(f: Int => Double): ai.dragonfly.math.vector.VectorValues = {
      val values: VectorValues = new VectorValues(dimension)
      for (i <- values.indices) values(i) = f(i)
      values
    }
  }

  type VectorValuesObject = VectorValues.type
}
