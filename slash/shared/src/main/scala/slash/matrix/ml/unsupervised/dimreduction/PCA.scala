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

package slash.matrix.ml.unsupervised.dimreduction

import slash.*

import slash.matrix.util.*
import slash.matrix.*
import slash.matrix.decomposition.SV
import slash.matrix.ml.data.*

import slash.vector.*
import Vec.*
import narr.*

import scala.language.implicitConversions

import scala.compiletime.ops.int.*

object PCA {

  // Create a PCA object from a set of data points
  def apply[M <: Int, N <:  Int](data: UnsupervisedData[M, N])(using ValueOf[M], ValueOf[N], N >= N =:= true): PCA[N] = {

    // arrange the matrix of centered points
    val Xc: Matrix[M, N] = Matrix[M, N](
      NArray.tabulate[Vec[N]](data.sampleSize)(
        (row:Int) => (data.example(row) - data.sampleMean)
      )
    )

    val m:Matrix[N, N] = (Xc.transpose * Xc) * (1.0 / data.sampleSize)

    new PCA[N](
      SV[N, N](m), // Compute Singular Value Decomposition
      data.sampleMean
    )
  }
}

case class PCA[N <: Int](svd: SV[N, N], mean: Vec[N])(using ValueOf[N]) {

  val dimension: Double = valueOf[N]

  lazy val Uᵀ:Matrix[N, N] = svd.U.transpose

  inline def getReducer[K <: Int](using ValueOf[K]): DimensionalityReducerPCA[N, K] = {
    DimensionalityReducerPCA[N, K](Matrix(Uᵀ.rowVectors.take(valueOf[K])), mean)
  }

  lazy val basisPairs: Seq[BasisPair[N]] = {
    val singularValues = svd.singularValues
    val arr: NArray[Vec[N]] = Uᵀ.rowVectors
    var pairs:Seq[BasisPair[N]] = Seq[BasisPair[N]]()
    var i:Int = 0; while (i < arr.length) {
      pairs = pairs :+ BasisPair[N]( singularValues(i), arr(i) )
      i += 1
    }
    pairs
  }
}

case class BasisPair[N <: Int](variance: Double, basisVector: Vec[N])(using ValueOf[N])

case class DimensionalityReducerPCA[N <: Int, K <: Int](Ak:Matrix[K, N], mean: Vec[N])(using ValueOf[N], ValueOf[K]) {

  /**
   * Reduce dimensionality of vector from domainDimension to rangeDimension
   * @param v domainDimensioned vector
   * @return rangeDimensioned vector
   */
  def apply(v:Vec[N]):Vec[K] = {
    (Ak * (v - mean).asColumnMatrix).values.asInstanceOf[Vec[K]]
  }

  def domainDimension:Int = mean.dimension

  def rangeDimension:Int = Ak.rows

  /**
   * Approximate inverse of dimensionality reduction
   *
   * @param v rangeDimensioned vector
   * @return rangeDimensioned vector
   */
  inline def unapply(v:Vec[K])(using ValueOf[K]):Vec[N] = (v.asRowMatrix * Ak).values.asInstanceOf[Vec[N]] + mean

}
