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

package slash.matrix.decomposition

import slash.matrix.*
import slash.matrix.util.MatrixNotSymmetricPositiveDefinite

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

  def apply[N <: Int](m:Mat[N, N])(using ValueOf[N]):Cholesky[N] = {
    // Initialize.
    val A = m.copy
    val n = A.rows
    val L = Mat.zeros[N, N]
    var isspd:Boolean = true
    // Main loop.
    var j:Int = 0; while (j < n) {
      //val Lrowj = L(j)
      var d = 0.0
      var k:Int = 0; while (k < j) {
        //val Lrowk = L(k)
        var s = 0.0
        var i:Int = 0; while (i < k) {
          //s += Lrowk(i) * Lrowj(i)
          s += L(k, i) * L(j, i)
          i += 1
        }
        s = (A(j, k) - s) / L(k, k)
        //Lrowj(k) = s
        L(j, k) = s
        d = d + s * s
        isspd = isspd & (A(k, j) == A(j, k))
        k += 1
      }
      d = A(j, j) - d
      isspd = isspd & (d > 0.0)
      L(j, j) = Math.sqrt(Math.max(d, 0.0))
      k = j + 1; while (k < n) { // recycling k
        L(j, k) = 0.0
        k += 1
      }
      j += 1
    }
    if (isspd) new Cholesky[N](L)
    else throw MatrixNotSymmetricPositiveDefinite[N, N](m)
  }
}

class Cholesky[N <: Int] private(val L: Mat[N, N])(using ValueOf[N]) {

  val mn:Int = valueOf[N]

  /** Solve A*X = B
    *
    * @param  B A Mat with as many rows as A and any number of columns.
    * @return X so that L*L'*X = B
    * @throws IllegalArgumentException  Mat row dimensions must agree.
    * @throws RuntimeException  Mat is not symmetric positive definite.
    */
  def solve[V <: Int](B: Mat[N, V])(using ValueOf[V]): Mat[N, V] = {

    // Copy right hand side.
    val X: Mat[N, V] = B.copy
    val nx: Int = B.columns
    // Solve L*Y = B;
    var k:Int = 0; while (k < mn) {
      var j:Int = 0; while (j < nx) {
        var i:Int = 0; while (i < k) {
          X(k, j) = X(k, j) - X(i, j) * L(k, i)
          i += 1
        }
        X(k, j) = X(k, j) / L(k, k)
        j += 1
      }
      k += 1
    }
    // Solve L'*thatMatrix = Y;
    k = mn - 1; while  (k > -1) { // recycling k
      var j:Int = 0; while (j < nx) {
        var i:Int = k + 1; while (i < mn) {
          X(k, j) = X(k, j) - X(i, j) * L(i, k)
          i += 1
        }
        X(k, j) = X(k, j) / L(k, k)
        j += 1
      }
      k -= 1
    }
    X
  }

}

