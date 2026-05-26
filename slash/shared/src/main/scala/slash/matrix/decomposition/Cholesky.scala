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

import narr.*
import slash.{dimensionCheck, squareInPlace}
import slash.matrix.*
import slash.exceptions.*
import slash.matrix.util.lindex

object CholeskySolver {

  def computeL(n:Int, m:NArray[Double]):NArray[Double] = {
    // Initialize.
    val A = NArray.copy[Double](m)
    val L = new NArray[Double](squareInPlace(n))
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
          s += L(lindex(k, i, n)) * L(lindex(j, i, n))
          i += 1
        }
        s = (A(lindex(j, k, n)) - s) / L(lindex(k, k, n))
        //Lrowj(k) = s
        L(lindex(j, k, n)) = s
        d = d + s * s
        isspd = isspd & (A(lindex(k, j, n)) == A(lindex(j, k, n)))
        k += 1
      }
      d = A(lindex(j, j, n)) - d
      isspd = isspd & (d > 0.0)
      L(lindex(j, j, n)) = Math.sqrt(Math.max(d, 0.0))
      k = j + 1; while (k < n) { // recycling k
        L(lindex(j, k, n)) = 0.0
        k += 1
      }
      j += 1
    }
    if (isspd) L
    else throw MatrixNotSymmetricPositiveDefinite(n, n)
  }


  /** Solve A*X = B
   *
   * @param B A Mat with as many rows as A and any number of columns.
   * @return X so that L*L'*X = B
   * @throws IllegalArgumentException Mat row dimensions must agree.
   * @throws RuntimeException         Mat is not symmetric positive definite.
   */
  def solve(mn:Int, lValues:NArray[Double], bColumns:Int, bValues: NArray[Double]): NArray[Double] = {

    // Copy right hand side.
    val X: NArray[Double] = NArray.copy[Double](bValues)
    val nx: Int = bColumns
    // Solve L*Y = B;
    var k: Int = 0;
    while (k < mn) {
      var j: Int = 0;
      while (j < nx) {
        var i: Int = 0;
        while (i < k) {
          X(lindex(k, j, bColumns)) = X(lindex(k, j, bColumns)) - X(lindex(i, j, bColumns)) * lValues(lindex(k, i, mn))
          i += 1
        }
        X(lindex(k, j, bColumns)) = X(lindex(k, j, bColumns)) / lValues(lindex(k, k, mn))
        j += 1
      }
      k += 1
    }
    // Solve L'*thatMatrix = Y;
    k = mn - 1;
    while (k > -1) { // recycling k
      var j: Int = 0;
      while (j < nx) {
        var i: Int = k + 1;
        while (i < mn) {
          X(lindex(k, j, bColumns)) = X(lindex(k, j, bColumns)) - X(lindex(i, j, bColumns)) * lValues(lindex(i, k, mn))
          i += 1
        }
        X(lindex(k, j, bColumns)) = X(lindex(k, j, bColumns)) / lValues(lindex(k, k, mn))
        j += 1
      }
      k -= 1
    }
    X
  }
}

object Cholesky {

  /** Cholesky Decomposition.
   * <P>
   * For a symmetric, positive definite matrix A, the Cholesky decomposition
   * is a lower triangular matrix L so that A = L*L'.
   */
  /** Cholesky algorithm for symmetric and positive definite matrix.
   * Structure to access L and isspd flag.
   *
   * @param m a square, symmetric, positive definite matrix.
   * @throws MatrixNotSymmetricPositiveDefinite exception if m is not a square, symmetric, positive definite matrix.
   */

  def apply[N <: Int](m:Mat[N, N])(using ValueOf[N]):Cholesky[N] = new Cholesky[N](
    new Mat[N,N](
      CholeskySolver.computeL(valueOf[N], m.values)
    )
  )
}

object RTCholesky {

  def apply(m: RTMat): RTCholesky = new RTCholesky(
    new RTMat(
      m.rowDimension,
      m.columnDimension,
      CholeskySolver.computeL(m.rowDimension, m.values)
    )
  )
}

class RTCholesky private (val L: RTMat) {
  dimensionCheck(L.rowDimension, L.columnDimension)

  val mn:Int = L.rowDimension

  def solve(B: RTMat): RTMat = {
    dimensionCheck(mn, B.rowDimension)
    new RTMat(
      mn, B.columnDimension, CholeskySolver.solve(mn, L.values, B.columnDimension, B.values)
    )
  }
}

class Cholesky[N <: Int] private (val L: Mat[N, N])(using ValueOf[N]) {

  val mn:Int = valueOf[N]

  /** Solve A*X = B
   *
   * @param  B A Mat with as many rows as A and any number of columns.
   * @return X so that L*L'*X = B
   * @throws IllegalArgumentException  Mat row dimensions must agree.
   * @throws RuntimeException  Mat is not symmetric positive definite.
   */
  def solve[V <: Int](B: Mat[N, V])(using ValueOf[V]): Mat[N, V] = new Mat[N, V](
    CholeskySolver.solve(mn, L.values, B.columnDimension, B.values )
  )
}