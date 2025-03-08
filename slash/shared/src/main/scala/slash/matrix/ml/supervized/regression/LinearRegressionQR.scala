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

import slash.matrix.decomposition.QR
import slash.matrix.*
class LinearRegressionQR[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]) extends LinearRegression[M, N] {
  override def estimateBeta(X: Mat[M, N], Y: Mat[M, 1]): Mat[N, 1] = QR[M, N](X).solve(Y)

}
