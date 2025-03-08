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
import slash.matrix.ml.data.SupervisedData
import slash.matrix.ml.supervized
import slash.matrix.ml.supervized.regression

import slash.stats.probability.distributions.EstimatedGaussian
import slash.vector.*

trait LinearRegression[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]) {

  def estimateBeta(X:Mat[M, N], Y:Mat[M, 1]): Mat[N, 1]

  def train(lrp:LinearRegressionProblem[M, N]): LinearRegressionModel[N] = {
    import lrp.*

    val A:Mat[N, 1] = estimateBeta(X, Y)

    val errors:Mat[M, 1] = (X * A) - Y

    var err:Double = 0.0
    var `err²`: Double = 0.0
    for (e <- errors.rowPackedArray) {
      val et:Double = e*e
      err = err + Math.sqrt(et)
      `err²` = `err²` + et
    }

    regression.LinearRegressionModel[N](
      A, lrp.mean, lrp.bias,
      err/size,
      1.0 - (`err²` / (`EstGaussian(Y)`.sampleVariance * `EstGaussian(Y)`.ℕ.toDouble))
    )
  }
}

trait LinearRegressionProblem[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]) {
  val sampleSize:Int = valueOf[M]
  val dimension:Int = valueOf[N]
  val X: Mat[M, N]
  val Y: Mat[M, 1]
  val bias:Double
  val mean:Vec[N]
  val `EstGaussian(Y)`: EstimatedGaussian
  def size:Double = `EstGaussian(Y)`.ℕ.toDouble
}

object LinearRegressionProblem {

  def apply[M <: Int, N <: Int](trainingData:SupervisedData[M, N])(using ValueOf[M], ValueOf[N]):LinearRegressionProblem[M, N] = {
    new LinearRegressionProblem[M, N] {
      override val X: Mat[M, N] = trainingData.X
      override val Y: Mat[M, 1] = trainingData.Y
      override val mean:Vec[N] = trainingData.sampleMean
      override val bias:Double = trainingData.rangeBias
      override val `EstGaussian(Y)`: EstimatedGaussian = trainingData.labelStats
    }
  }
}


