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


import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.decomposition.SV

import scala.compiletime.ops.int.*

class LinearRegressionSVD[M <: Int, N <: Int](using ValueOf[M], ValueOf[N], M >= N =:= true) extends LinearRegression[M, N] {

  override def estimateBeta(X: Matrix[M, N], Y: Matrix[M, 1]): Matrix[N, 1] = {
    // Â = VS⁻ⁱUᵀ * Y
    val svd: SV[M, N] = SV[M, N](X)
    //svd.getV() * (svd.getS_Inverse() * svd.getU().transpose) * Y
    svd.V.times(svd.S_inverse.times(svd.U.transpose)).times(Y)
  }


}
