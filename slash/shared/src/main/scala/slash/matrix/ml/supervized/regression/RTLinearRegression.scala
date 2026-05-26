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

package slash.matrix.ml.supervized.regression

import slash.matrix.*
import slash.matrix.ml.data.RTSupervisedData
import slash.matrix.ml.supervized
import slash.matrix.ml.supervized.regression

import slash.stats.probability.distributions.EstimatedGaussian
import slash.vector.runtime.*

trait RTLinearRegression {

  def estimateBeta(X:RTMat, Y:RTMat): RTMat

  def train(lrp:RTLinearRegressionProblem): RTLinearRegressionModel = {
    import lrp.*

    val A:RTMat = estimateBeta(X, Y)

    val errors:RTMat = (X * A) - Y

    var err:Double = 0.0
    var `err²`: Double = 0.0
    for (e <- errors.rowPackedArray) {
      val et:Double = e*e
      err = err + Math.sqrt(et)
      `err²` = `err²` + et
    }

    regression.RTLinearRegressionModel(
      A, lrp.mean, lrp.bias,
      err/size,
      1.0 - (`err²` / (`EstGaussian(Y)`.sampleVariance * `EstGaussian(Y)`.ℕ.toDouble))
    )
  }
}

trait RTLinearRegressionProblem {
  val sampleSize:Int
  val dimension:Int
  val X: RTMat
  val Y: RTMat
  val bias:Double
  val mean:RTVec
  val `EstGaussian(Y)`: EstimatedGaussian
  def size:Double = `EstGaussian(Y)`.ℕ.toDouble
}

object RTLinearRegressionProblem {

  def apply(trainingData:RTSupervisedData):RTLinearRegressionProblem = {
    new RTLinearRegressionProblem {
      override val sampleSize:Int = trainingData.sampleSize
      override val dimension:Int = trainingData.dimension
      override val X: RTMat = trainingData.X
      override val Y: RTMat = trainingData.Y
      override val mean:RTVec = trainingData.sampleMean
      override val bias:Double = trainingData.rangeBias
      override val `EstGaussian(Y)`: EstimatedGaussian = trainingData.labelStats
    }
  }
}