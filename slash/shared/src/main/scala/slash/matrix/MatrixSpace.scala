/*
 * Copyright 2023 dragonfly.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package slash.matrix

import narr.NArray
import slash.Random
import slash.vector.*

import scala.compiletime.ops.int.*

object MatrixSpace {

  def apply(rowDimension:Int, columnDimension:Int):MatrixSpace[rowDimension.type, columnDimension.type] = apply(
  VectorSpace(rowDimension),
  VectorSpace(columnDimension)
  )

  def apply[M <: Int, N <: Int](rowVectorSpace:VectorSpace[M], columnVectorSpace:VectorSpace[N]):MatrixSpace[M, N] = {
    new MatrixSpace[M, N](rowVectorSpace, columnVectorSpace)
  }

}

class MatrixSpace[M0 <: Int, N0 <: Int](val rowVectorSpace:VectorSpace[M0], val columnVectorSpace:VectorSpace[N0]) {

  val rowDimension:Int = rowVectorSpace.dimension
  val columnDimension:Int = columnVectorSpace.dimension

  opaque type M <: Int = rowVectorSpace.N
  opaque type N <: Int = columnVectorSpace.N
  opaque type MN <: Int = M * N

  inline def apply(values: NArray[Double]): Mat[M, N] = Mat[M, N](values)

  inline def copyFrom(values: NArray[Double]): Mat[M, N] = Mat[M, N](narr.copy[Double](values))

  inline def fromVec(v: Vec[MN]): Mat[M, N] = apply(v.asInstanceOf[NArray[Double]])

  inline def copyFromVec(v: Vec[MN]): Mat[M, N] = fromVec(v.copy)


  inline def identity: Mat[M, N] = diagonal(1.0)

  inline def diagonal(value:Double): Mat[M, N] = Mat.diagonal[M, N](value)

  inline def fill(d: Double): Mat[M, N] = Mat.fill[M, N](d)

  inline def zeros: Mat[M, N] = Mat.zeros[M, N]

  inline def ones: Mat[M, N] = Mat.ones[M, N]

  import slash.Random.nextMatrix

  inline def random: Mat[M, N] = Mat.random[M, N](Random.defaultRandom)
  inline def random(r: scala.util.Random): Mat[M, N] = Mat.random[M, N](r)

  inline def random(
    interval: slash.interval.Interval[Double],
    r: scala.util.Random = Random.defaultRandom
  ): Mat[M, N] = Mat.random[M, N](interval, r)

  inline def random(
    minNorm: Double,
    normMAX: Double
  ): Mat[M, N] = random(minNorm, normMAX, Random.defaultRandom)

  inline def random(
     minNorm: Double,
     normMAX: Double,
     r: scala.util.Random
   ): Mat[M, N] = r.nextMatrix[M, N](minNorm, normMAX)

}
