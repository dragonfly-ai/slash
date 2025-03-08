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
import slash.vector.*

/**
 * @param A
 * @param mean
 * @param bias
 * @param standardError
 * @param `R²` Coefficient of determination =
 */


case class LinearRegressionModel[N <: Int](A: Mat[N, 1], mean: Vec[N], bias: Double, standardError: Double, `R²`: Double) {
  val a: Vec[N] = A.copy.values.asInstanceOf[Vec[N]]

  def apply(x: Vec[N]): Double = (a dot (x - mean)) + bias
  //  def apply(X:Mat):Mat = {
  //    X.times(A)
  //  }

  override def toString: String = s"LinearRegressionModel(\n\t\tA = ${A.dim},\n\t\tmean = ${mean.render()},\n\t\tbias = $bias,\n\t\tstandardError = $standardError,\n\t\tR² = ${`R²`}\n\t)"
}
