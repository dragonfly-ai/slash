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

import slash.matrix.*
import slash.matrix.decomposition.RTSV
import slash.matrix.ml.data.*

import slash.vector.*
import runtime.*
import narr.*

import scala.language.implicitConversions

object RTPCA {

  // Create a PCA object from a set of data points
  def apply(data: RTUnsupervisedData): RTPCA = {

    // arrange the matrix of centered points
    val Xc: RTMat = RTMat(
      NArray.tabulate[RTVec](data.sampleSize)(
        (row:Int) => data.example(row) - data.sampleMean
      )
    )

    val m:RTMat = (Xc.transpose * Xc) * (1.0 / data.sampleSize)

    new RTPCA(
      RTSV(m), // Compute Singular Value Decomposition
      data.sampleMean
    )
  }
}

case class RTPCA(svd: RTSV, mean: RTVec) {

  val dimension: Int = mean.dimension

  lazy val Uᵀ:RTMat = svd.U.transpose

  inline def getReducer(k:Int): RTDimensionalityReducerPCA = {
    RTDimensionalityReducerPCA(RTMat(Uᵀ.rowVectors.take(k)), mean)
  }

  lazy val basisPairs: Seq[RTBasisPair] = {
    val singularValues = svd.singularValues
    val arr: NArray[RTVec] = Uᵀ.rowVectors
    var pairs:Seq[RTBasisPair] = Seq[RTBasisPair]()
    var i:Int = 0
    while (i < arr.length) {
      pairs = pairs :+ RTBasisPair( singularValues(i), arr(i) )
      i += 1
    }
    pairs
  }
}

case class RTBasisPair(variance: Double, basisVector: RTVec)

case class RTDimensionalityReducerPCA(Ak:RTMat, mean: RTVec) {

  /**
   * Reduce dimensionality of vector from domainDimension to rangeDimension
   * @param v domainDimensioned vector
   * @return rangeDimensioned vector
   */
  def apply(v:RTVec):RTVec = {
    (Ak * (v - mean).asColumnMatrix).values.asInstanceOf[RTVec]
  }

  def domainDimension:Int = mean.dimension

  def rangeDimension:Int = Ak.rowDimension

  /**
   * Approximate inverse of dimensionality reduction
   *
   * @param v rangeDimensioned vector
   * @return rangeDimensioned vector
   */
  inline def unapply(v:RTVec):RTVec = (v.asRowMatrix * Ak).values.asInstanceOf[RTVec] + mean

}
