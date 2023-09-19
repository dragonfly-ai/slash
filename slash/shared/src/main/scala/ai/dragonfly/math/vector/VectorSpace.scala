package ai.dragonfly.math.vector

import narr.NArray

import scala.compiletime.ops.int.*

object VectorSpace {
  def apply(dimension:Int):VectorSpace[dimension.type] = new VectorSpace[dimension.type]
}


class VectorSpace[N0 <: Int](using dt: ValueOf[N0]) {

  val dimension:Int = dt.value
  dimensionCheck(dimension, dt.value)

  opaque type N <: Int = N0

  given n: ValueOf[N] = dt

  inline def apply(a: NArray[Double]): Vec[N] = {
    dimensionCheck(a.length, dimension)
    a.asInstanceOf[Vec[N]]
  }

  inline def apply(d: Double*): Vec[N] = Vec.apply[N](d: _*)

  inline def tabulate(f: (i: Int) => Double): Vec[N] = apply(NArray.tabulate[Double](dimension)(f))

  inline def fill(d: Double): Vec[N] = apply(NArray.fill[Double](dimension)(d))

  inline def zeros: Vec[N] = apply(NArray.fill[Double](dimension)(0.0))

  inline def ones: Vec[N] = apply(NArray.fill[Double](dimension)(1.0))

  import ai.dragonfly.math.Random.nextVec

  inline def random(
    MAX: Double = 1.0,
    min: Double = 0.0,
    r: scala.util.Random = ai.dragonfly.math.Random.defaultRandom
  ): Vec[N] = r.nextVec[N](min, MAX)

}
