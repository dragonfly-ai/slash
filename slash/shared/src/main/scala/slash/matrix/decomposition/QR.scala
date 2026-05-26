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

import slash.vector.*
import slash.matrix.*
import narr.*
import slash.dimensionCheck
import slash.matrix.util.lindex
import slash.vector.runtime.RTVec

import scala.math.hypot
//import slash.matrix.decomposition.SV.hypot

object QRSolver {
  def apply(rows:Int, columns:Int, M: NArray[Double]): (NArray[Double], NArray[Double]) = {
    // Initialize.
    val qr: NArray[Double] = NArray.copy[Double](M)
    val Rdiag: NArray[Double] = new NArray[Double](columns)

    // Main loop.
    var k: Int = 0;
    while (k < columns) { // Compute 2-norm of k-th column without under/overflow.
      var nrm: Double = 0.0
      var i: Int = k;
      while (i < rows) {
        nrm = hypot(nrm, qr(lindex(i, k, columns)))
        i += 1
      }
      if (nrm != 0.0) { // Form k-th Householder vector.
        if (qr(lindex(k, k, columns)) < 0) nrm = -nrm
        i = k;
        while (i < rows) { // recycling
          qr(lindex(i, k, columns)) /= nrm
          i += 1
        }
        qr(lindex(k, k, columns)) += 1.0
        // Apply transformation to remaining columns.
        var j: Int = k + 1;
        while (j < columns) {
          var s = 0.0
          var i0: Int = k;
          while (i0 < rows) {
            s += qr(lindex(i0, k, columns)) * qr(lindex(i0, j, columns))
            i0 += 1
          }
          s = -s / qr(lindex(k, k, columns))
          var i1: Int = k;
          while (i1 < rows) {
            qr(lindex(i1, j, columns)) += s * qr(lindex(i1, k, columns))
            i1 += 1
          }
          j += 1
        }
      }
      Rdiag(k) = -nrm
      k += 1
    }
//    new QR(qr, Rdiag)
    (qr, Rdiag)
  }

  /** Is the matrix full rank?
   *
   * @return true if R, and hence A, has full rank.
   */
  def fullRank(Rdiag:NArray[Double]): Boolean = {
    var i: Int = 0
    while (i < Rdiag.length && Rdiag(i) != 0.0) i += 1
    i == Rdiag.length
  }

  /** Return the Householder vectors
   *
   * @return Lower trapezoidal matrix whose columns define the reflections
   */
  def H(rows:Int, columns:Int, qr:NArray[Double]): NArray[Double] = {
    val out: NArray[Double] = new NArray[Double](rows * columns)
    var i: Int = 0
    var r: Int = 0
    while (r < rows) {
      var c: Int = 0
      while (c < columns) {
        out(i) = if (r >= c) qr(lindex(r, c, columns)) else 0.0
        i += 1
        c += 1
      }
      r += 1
    }
    out
  }


  /** Return the upper triangular factor
   *
   * @return R
   */
  def R(columns:Int, qr:NArray[Double], Rdiag:NArray[Double]): NArray[Double] = {
    val out: NArray[Double] = new NArray[Double](slash.squareInPlace(columns))
    var i: Int = 0
    var r: Int = 0
    while (r < columns) {
      var c: Int = 0
      while (c < columns) {
        out(i) = {
          if (r < c) qr(lindex(r, c, columns))
          else if (r == c) Rdiag(r)
          else 0.0
        }
        i += 1
        c += 1
      }
      r += 1
    }
    out
  }

  def Q(rows:Int, columns:Int, QR:NArray[Double]): NArray[Double] = {
    val X = new NArray[Double](rows * columns)
    var k:Int = columns - 1
    while (k > -1) {
      var i:Int = 0
      while (i < rows) {
        X(lindex(i, k, columns)) = 0.0
        i += 1
      }
      X(lindex(k, k, columns)) = 1.0
      var j:Int = k
      while (j < columns) {
        if (QR(lindex(k, k, columns)) != 0) {
          var s = 0.0
          i = k
          while (i < rows) {  // recycling i
            s += QR(lindex(i, k, columns)) * X(lindex(i, j, columns))
            i += 1
          }
          s = -s / QR(lindex(k, k, columns))
          i = k
          while (i < rows) {  // recycling i
            X(lindex(i, j, columns)) += s * QR(lindex(i, k, columns))
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
   * @param B A Mat with as many rows as A and any number of columns.
   * @return X that minimizes the two norm of Q*R*X-B.
   * @throws IllegalArgumentException  Mat row dimensions must agree.
   * @throws RuntimeException  Mat is rank deficient.
   */
//  def solve[V <: Int](B: Mat[M, V])(using ValueOf[V]): Mat[N, V] = {
  def solve(rows:Int, columns:Int, QR:NArray[Double], Rdiag:NArray[Double], bColumns:Int, B: NArray[Double]): NArray[Double] = {

    val out:NArray[Double] = NArray.copy[Double](B)
    // Copy right hand side
    val nx = bColumns

    // Compute Y = transpose(Q)*B
    var k:Int = 0
    while (k < columns) {
      var j:Int = 0
      while (j < nx) {
        var s:Double = 0.0
        var i:Int = k
        while (i < rows) {
          s += QR(lindex(i, k, columns)) * out(lindex(i, j, bColumns))
          i += 1
        }
        s = -s / QR(lindex(k, k, columns))
        i = k
        while (i < rows) { // recycling i
          out(lindex(i, j, bColumns)) += s * QR(lindex(i, k, columns))
          i += 1
        }
        j += 1
      }
      k += 1
    }

    // Solve R*thatMatrix = Y;
    k = columns - 1
    while (k > -1) { // recycling k
      var j:Int = 0
      while (j < nx) {
        out(lindex(k, j, bColumns)) /= Rdiag(k)
        j += 1
      }
      var i:Int = 0
      while (i < k) {
        j = 0
        while (j < nx) {
          out(lindex(i, j, bColumns)) -= out(lindex(k, j, bColumns)) * QR(lindex(i, k, columns))
          j += 1
        }
        i += 1
      }
      k -= 1
    }
    //println(s"slash.matrix.util.subMatrix($columns, ${out.length}, 0, 0, $columns, $bColumns)")
    if (rows == columns) out
    else {
      //println(s"subMatrix($columns, ${out.length}, 0, 0, $columns, $bColumns)")
      slash.matrix.util.subMatrix(bColumns, out, 0, 0, columns, bColumns)
    }
    //B.subMatrix[N, V](0, 0)
  }

}

object QR {

  def apply[M <: Int, N <: Int](M: Mat[M, N])(using ValueOf[M], ValueOf[N]): QR[M, N] = {
    val temp:(NArray[Double], NArray[Double]) = QRSolver(M.rows, M.columns, M.values)
    new QR(new Mat[M, N](temp._1), Vec[N](temp._2))
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

/**
 * QR Decomposition, computed by Householder reflections.
 * Structure to access R and the Householder vectors and compute Q.
 *
 * @param QR
 * @param Rdiag
 * @param x$3
 * @param x$4
 * @tparam M
 * @tparam N
 */

class QR[M <: Int, N <: Int] private (
  val QR: Mat[M, N], val Rdiag: Vec[N]
)(using ValueOf[M], ValueOf[N]) {

  val rows:Int = valueOf[M]
  val columns:Int = valueOf[N]

  /** Is the matrix full rank?
    *
    * @return true if R, and hence A, has full rank.
    */
  lazy val fullRank: Boolean = QRSolver.fullRank(Rdiag.asNativeArray)

  /** Return the Householder vectors
    *
    * @return Lower trapezoidal matrix whose columns define the reflections
    */
  lazy val H: Mat[M, N] = Mat[M, N](QRSolver.H(rows, columns, QR.values))

  /** Return the upper triangular factor
    *
    * @return R
    */
  lazy val R: Mat[N, N] = Mat[N, N](QRSolver.R(columns, QR.values, Rdiag.asNativeArray))

  /** Generate and return the (economy-sized) orthogonal factor
    *
    * @return Q
    */
  lazy val Q: Mat[M, N] = Mat[M, N](QRSolver.Q(rows, columns, QR.values))

  /** Least squares solution of A*X = B
    *
    * @param B A Mat with as many rows as A and any number of columns.
    * @return X that minimizes the two norm of Q*R*X-B.
    * @throws IllegalArgumentException  Mat row dimensions must agree.
    * @throws RuntimeException  Mat is rank deficient.
    */
  def solve[V <: Int](B: Mat[M, V])(using ValueOf[V]): Mat[N, V] = {
    if (fullRank) {
      Mat[N, V](QRSolver.solve(rows, columns, QR.values, Rdiag.asNativeArray, B.columnDimension, B.values))
    } else throw new RuntimeException("Mat is rank deficient.")
  }
}


object RTQR {

  def apply(M: RTMat): RTQR = {
    val temp:(NArray[Double], NArray[Double]) = QRSolver(M.rowDimension, M.columnDimension, M.values)
    new RTQR(new RTMat(M.rowDimension, M.columnDimension, temp._1), RTVec(temp._2))
  }

}

/**
 * QR Decomposition, computed by Householder reflections.
 * Structure to access R and the Householder vectors and compute Q.
 *
 * @param QR
 * @param Rdiag
 * @param x$3
 * @param x$4
 * @tparam M
 * @tparam N
 */

class RTQR private (val QR: RTMat, val Rdiag: RTVec){

  dimensionCheck(QR.columnDimension, Rdiag.dimension)

  val rows:Int = QR.rowDimension
  val columns:Int = QR.columnDimension

  /** Is the matrix full rank?
   *
   * @return true if R, and hence A, has full rank.
   */
  lazy val fullRank: Boolean = QRSolver.fullRank(Rdiag.asNativeArray)

  /** Return the Householder vectors
   *
   * @return Lower trapezoidal matrix whose columns define the reflections
   */
  lazy val H: RTMat = RTMat(rows, columns, QRSolver.H(rows, columns, QR.values))

  /** Return the upper triangular factor
   *
   * @return R [NxN]
   */
  lazy val R: RTMat = RTMat(columns, columns, QRSolver.R(columns, QR.values, Rdiag.asNativeArray))

  /** Generate and return the (economy-sized) orthogonal factor
   *
   * @return Q
   */
  lazy val Q: RTMat = RTMat(rows, columns, QRSolver.Q(rows, columns, QR.values))

  /** Least squares solution of A*X = B
   *
   * @param B A Mat with as many rows as A and any number of columns.
   * @return X that minimizes the two norm of Q*R*X-B.
   * @throws IllegalArgumentException  Mat row dimensions must agree.
   * @throws RuntimeException  Mat is rank deficient.
   */
  def solve(B: RTMat): RTMat = {
    dimensionCheck(columns, B.rowDimension)
    if (fullRank) {
      RTMat(
        columns,
        B.columnDimension,
        QRSolver.solve(rows, columns, QR.values, Rdiag.asNativeArray, B.columnDimension, B.values)
      )
    } else throw new RuntimeException("Mat is rank deficient.")
  }

}