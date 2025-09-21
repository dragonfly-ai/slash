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

package slash.matrixf

import narr.NArray
import slash.Random
import slash.Random.nextMatrixF
import slash.vectorf.*
import slash.vectorf.vectorf.*

import scala.compiletime.ops.int.*

object MatrixFSpace {

  def apply(rowDimension:Int, columnDimension:Int):MatrixFSpace[rowDimension.type, columnDimension.type] = apply(
  VectorFSpace(rowDimension),
  VectorFSpace(columnDimension)
  )

  def apply[M <: Int, N <: Int](rowVectorFSpace:VectorFSpace[M], columnVectorFSpace:VectorFSpace[N]):MatrixFSpace[M, N] = {
    new MatrixFSpace[M, N](rowVectorFSpace, columnVectorFSpace)
  }

}

class MatrixFSpace[M0 <: Int, N0 <: Int](val rowVectorFSpace:VectorFSpace[M0], val columnVectorFSpace:VectorFSpace[N0]) {

  val rowDimension:Int = rowVectorFSpace.dimension
  val columnDimension:Int = columnVectorFSpace.dimension
  val MxN:Int = rowDimension * columnDimension

  opaque type M <: Int = rowVectorFSpace.N
  opaque type N <: Int = columnVectorFSpace.N
  opaque type MN <: Int = M * N

  given m: ValueOf[M] = rowVectorFSpace.n
  given n: ValueOf[N] = columnVectorFSpace.n


  inline def apply(values: NArray[Float]): MatF[M, N] = MatF[M, N](values)

  inline def copyFrom(values: NArray[Float]): MatF[M, N] = MatF[M, N](narr.copy[Float](values))

  inline def fromVec(v: VecF[MN]): MatF[M, N] = apply(v.asInstanceOf[NArray[Float]])

  inline def copyFromVec(v: VecF[MN]): MatF[M, N] = fromVec(v.copy)


  inline def identity: MatF[M, N] = diagonal(1.0)

  inline def diagonal(value:Float): MatF[M, N] = MatF.diagonal[M, N](value)

  inline def fill(d: Float): MatF[M, N] = MatF.fill[M, N](d)

  inline def zeros: MatF[M, N] = MatF.zeros[M, N]

  inline def ones: MatF[M, N] = MatF.ones[M, N]

  inline def random: MatF[M, N] = MatF.random[M, N](Random.defaultRandom)
  inline def random(r: scala.util.Random): MatF[M, N] = MatF.random[M, N](r)

  inline def random(
    interval: slash.interval.FloatInterval,
    r: scala.util.Random = Random.defaultRandom
  ): MatF[M, N] = MatF.random[M, N](interval, r)

  inline def random(
    minNorm: Float,
    normMAX: Float
  ): MatF[M, N] = random(minNorm, normMAX, Random.defaultRandom)

  inline def random(
     minNorm: Float,
     normMAX: Float,
     r: scala.util.Random
   ): MatF[M, N] = r.nextMatrixF[M, N](minNorm, normMAX)

}
