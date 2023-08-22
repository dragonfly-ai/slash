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

package ai.dragonfly.math.matrix.ml.supervized.regression

import ai.dragonfly.math.interval.Interval.*
import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.ml.data.{StaticSupervisedData, SupervisedData}
import ai.dragonfly.math.matrix.ml.supervized
import ai.dragonfly.math.matrix.ml.supervized.regression
import ai.dragonfly.math.stats.LabeledVec
import ai.dragonfly.math.stats.probability.distributions.{EstimatedGaussian, stream}
import ai.dragonfly.math.vector.*

trait LinearRegression[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]) {

  def estimateBeta(X:Matrix[M, N], Y:Matrix[M, 1]): Matrix[N, 1]

  def train(lrp:LinearRegressionProblem[M, N]): LinearRegressionModel[N] = {
    import lrp.*

    val A:Matrix[N, 1] = estimateBeta(X, Y)

    val errors:Matrix[M, 1] = (X * A) - Y

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
      1.0 - (`err²` / (`EstGaussian(Y)`.sampleVariance * `EstGaussian(Y)`.ℕ))
    )
  }
}

trait LinearRegressionProblem[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]) {
  val sampleSize:Int = valueOf[M]
  val dimension:Int = valueOf[N]
  val X: Matrix[M, N]
  val Y: Matrix[M, 1]
  val bias:Double
  val mean:Vec[N]
  val `EstGaussian(Y)`: EstimatedGaussian
  def size:Double = `EstGaussian(Y)`.ℕ
}

object LinearRegressionProblem {

  def apply[M <: Int, N <: Int](trainingData:SupervisedData[M, N])(using ValueOf[M], ValueOf[N]):LinearRegressionProblem[M, N] = {
    new LinearRegressionProblem[M, N] {
      override val X: Matrix[M, N] = trainingData.X
      override val Y: Matrix[M, 1] = trainingData.Y
      override val mean:Vec[N] = trainingData.sampleMean
      override val bias:Double = trainingData.rangeBias
      override val `EstGaussian(Y)`: EstimatedGaussian = trainingData.labelStats
    }
  }
}


