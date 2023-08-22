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

package ai.dragonfly.math.matrix.decomposition

import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.util.MatrixNotSymmetricPositiveDefinite
import narr.*

object Cholesky {

  /** Cholesky Decomposition.
   * <P>
   * For a symmetric, positive definite matrix A, the Cholesky decomposition
   * is an lower triangular matrix L so that A = L*L'.
   */
  /** Cholesky algorithm for symmetric and positive definite matrix.
   * Structure to access L and isspd flag.
   *
   * @param m a square, symmetric, positive definite matrix.
   * @throws MatrixNotSymmetricPositiveDefinite exception if m is not a square, symmetric, positive definite matrix.
   */

  def apply[N <: Int](m:Matrix[N, N])(using ValueOf[N]):Cholesky[N] = {
    // Initialize.
    val A = m.copyValues
    val n = A.length
    val L = Matrix.zeros[N, N].values
    var isspd:Boolean = true
    // Main loop.
    var j:Int = 0; while (j < n) {
      val Lrowj = L(j)
      var d = 0.0
      var k:Int = 0; while (k < j) {
        val Lrowk = L(k)
        var s = 0.0
        var i:Int = 0; while (i < k) {
          s += Lrowk(i) * Lrowj(i)
          i += 1
        }
        s = (A(j)(k) - s) / L(k)(k)
        Lrowj(k) = s
        d = d + s * s
        isspd = isspd & (A(k)(j) == A(j)(k))
        k += 1
      }
      d = A(j)(j) - d
      isspd = isspd & (d > 0.0)
      L(j)(j) = Math.sqrt(Math.max(d, 0.0))
      k = j + 1; while (k < n) { // recycling k
        L(j)(k) = 0.0
        k += 1
      }
      j += 1
    }
    if (isspd) new Cholesky(Matrix[N, N](L))
    else throw MatrixNotSymmetricPositiveDefinite[N, N](m)
  }
}

class Cholesky[N <: Int] private(val L: Matrix[N, N])(using ValueOf[N]) {

  val mn:Int = valueOf[N]

  /** Solve A*X = B
    *
    * @param  B A Matrix with as many rows as A and any number of columns.
    * @return X so that L*L'*X = B
    * @throws IllegalArgumentException  Matrix row dimensions must agree.
    * @throws RuntimeException  Matrix is not symmetric positive definite.
    */
  def solve[V <: Int](B: Matrix[N, V])(using ValueOf[V]): Matrix[N, V] = {

    // Copy right hand side.
    val X: NArray[NArray[Double]] = B.copyValues
    val nx: Int = B.columns
    // Solve L*Y = B;
    var k:Int = 0; while (k < mn) {
      var j:Int = 0; while (j < nx) {
        var i:Int = 0; while (i < k) {
          X(k)(j) = X(k)(j) - X(i)(j) * L(k, i)
          i += 1
        }
        X(k)(j) = X(k)(j) / L(k, k)
        j += 1
      }
      k += 1
    }
    // Solve L'*X = Y;
    k = mn - 1; while  (k > -1) { // recycling k
      var j:Int = 0; while (j < nx) {
        var i:Int = k + 1; while (i < mn) {
          X(k)(j) = X(k)(j) - X(i)(j) * L(i, k)
          i += 1
        }
        X(k)(j) = X(k)(j) / L(k, k)
        j += 1
      }
      k -= 1
    }
    Matrix[N, V](X)
  }

}

