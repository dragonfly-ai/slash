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
import slash.vector.Vec
import narr.*

import scala.math.hypot
//import slash.matrix.decomposition.SV.hypot

object QR {

  def apply[M <: Int, N <: Int](M: Matrix[M, N])(using ValueOf[M], ValueOf[N]): QR[M, N] = {
    // Initialize.
    val qr:Matrix[M, N] = M.copy
    val rows:Int = M.rows
    val columns:Int = M.columns
    val Rdiag:Vec[N] = Vec.fill[N](0.0)

    // Main loop.
    var k:Int = 0; while (k < columns) { // Compute 2-norm of k-th column without under/overflow.
      var nrm:Double = 0.0
      var i:Int = k; while (i < rows) {
        nrm = hypot(nrm, qr(i, k))
        i += 1
      }
      if (nrm != 0.0) { // Form k-th Householder vector.
        if (qr(k, k) < 0) nrm = -nrm
        i = k; while (i < rows) {  // recycling
          qr(i, k) /= nrm
          i += 1
        }
        qr(k, k) += 1.0
        // Apply transformation to remaining columns.
        var j:Int = k + 1; while (j < columns) {
          var s = 0.0
          var i0:Int = k; while (i0 < rows) {
            s += qr(i0, k) * qr(i0, j)
            i0 += 1
          }
          s = -s / qr(k, k)
          var i1:Int = k; while (i1 < rows) {
            qr(i1, j) += s * qr(i1, k)
            i1 += 1
          }
          j += 1
        }
      }
      Rdiag(k) = -nrm
      k += 1
    }
    new QR(qr, Rdiag)
  }

}

/** QR Decomposition.
  * <P>
  * For an rows-by-columns matrix A with rows >= columns, the QR decomposition is an rows-by-columns
  * orthogonal matrix Q and an columns-by-columns upper triangular matrix R so that
  * A = Q*R.
  * <P>
  * The QR decompostion always exists, even if the matrix does not have
  * full rank, so the constructor will never fail.  The primary use of the
  * QR decomposition is in the least squares solution of nonsquare systems
  * of simultaneous linear equations.  This will fail if isFullRank()
  * returns false.
  */
/** QR Decomposition, computed by Householder reflections.
  * Structure to access R and the Householder vectors and compute Q.
  *
  * @param A Rectangular matrix
  */

class QR[M <: Int, N <: Int] private (
  val QR: Matrix[M, N], val Rdiag: Vec[N]
)(using ValueOf[M], ValueOf[N]) {

  val rows:Int = valueOf[M]
  val columns:Int = valueOf[N]

  /** Is the matrix full rank?
    *
    * @return true if R, and hence A, has full rank.
    */
  def isFullRank: Boolean = {
    var i:Int = 0
    while (i < columns && Rdiag(i) != 0.0) i += 1
    i == columns
  }

  /** Return the Householder vectors
    *
    * @return Lower trapezoidal matrix whose columns define the reflections
    */
  def H: Matrix[M, N] = {
    val values: NArray[Double] = new NArray[Double](rows * columns)
    var i: Int = 0
    var r: Int = 0;
    while (r < rows) {
      var c: Int = 0;
      while (c < columns) {
        values(i) = if (r >= c) QR(r, c) else 0.0
        i += 1
        c += 1
      }
      r += 1
    }
    Matrix[M, N](values)
  }

  /** Return the upper triangular factor
    *
    * @return R
    */
  def R: Matrix[N, N] = {
    val values: NArray[Double] = new NArray[Double](slash.squareInPlace(columns))
    var i: Int = 0
    var r: Int = 0;
    while (r < columns) {
      var c: Int = 0;
      while (c < columns) {
        values(i) = {
          if (r < c) QR(r, c)
          else if (r == c) Rdiag(r)
          else 0.0
        }
        i += 1
        c += 1
      }
      r += 1
    }
    Matrix[N, N](values)
  }

  /** Generate and return the (economy-sized) orthogonal factor
    *
    * @return Q
    */
  def Q: Matrix[M, N] = {
    val X = Matrix.zeros[M, N]
    var k:Int = columns - 1; while (k > -1) {
      var i:Int = 0; while (i < rows) {
        X(i, k) = 0.0
        i += 1
      }
      X(k, k) = 1.0
      var j:Int = k; while (j < columns) {
        if (QR(k, k) != 0) {
          var s = 0.0
          i = k; while (i < rows) {  // recycling i
            s += QR(i, k) * X(i, j)
            i += 1
          }
          s = -s / QR(k, k)
          i = k; while (i < rows) {  // recycling i
            X(i, j) += s * QR(i, k)
            i += 1
          }
        }
        j += 1
      }
      k -= 1
    }
    X
  }

  /** Least squares solution of A*X = B
    *
    * @param B A Matrix with as many rows as A and any number of columns.
    * @return X that minimizes the two norm of Q*R*X-B.
    * @throws IllegalArgumentException  Matrix row dimensions must agree.
    * @throws RuntimeException  Matrix is rank deficient.
    */
  def solve[V <: Int](B: Matrix[M, V])(using ValueOf[V]): Matrix[N, V] = {
    if (!this.isFullRank) throw new RuntimeException("Matrix is rank deficient.")

    // Copy right hand side
    val nx = B.columns

    // Compute Y = transpose(Q)*B
    var k:Int = 0; while (k < columns) {
      var j:Int = 0; while (j < nx) {
        var s:Double = 0.0
        var i:Int = k; while (i < rows) {
          s += QR(i, k) * B(i, j)
          i += 1
        }
        s = -s / QR(k, k)
        i = k; while (i < rows) { // recycling i
          B(i, j) += s * QR(i, k)
          i += 1
        }
        j += 1
      }
      k += 1
    }

    // Solve R*thatMatrix = Y;
    k = columns - 1; while (k > -1) { // recycling k
      var j:Int = 0; while (j < nx) {
        B(k, j) /= Rdiag(k)
        j += 1
      }
      var i:Int = 0; while (i < k) {
        j = 0; while (j < nx) {
          B(i, j) -= B(k, j) * QR(i, k)
          j += 1
        }
        i += 1
      }
      k -= 1
    }

    B.subMatrix[N, V](0, 0)
  }

}