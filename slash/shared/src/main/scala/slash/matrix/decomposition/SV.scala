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
import slash.vector.*
import slash.matrix.*

import scala.math.hypot
import slash.matrix.util.lindex
import slash.vector.runtime.RTVec

import scala.compiletime.ops.int.*

object SVSolver {

  //  /** This hypot function is a direct port from Jama.util.maths.
  //   *
  //   * this matrix library uses scala.math.hypot from the Java standard lib, but doing so introduces a small difference
  //   * between the Jama reference implementation and this one.  In order to verify equivalence between this and the
  //   * reference, uncomment and import this function instead of the one from scala.math.hypot
  //   *
  //   * @param a
  //   * @param b
  //   * @return
  //   */
  //  def hypot(a: Double, b: Double): Double = {
  //    var r = .0
  //    if (Math.abs(a) > Math.abs(b)) {
  //      r = b / a
  //      r = Math.abs(a) * Math.sqrt(1 + r * r)
  //    }
  //    else if (b != 0) {
  //      r = a / b
  //      r = Math.abs(b) * Math.sqrt(1 + r * r)
  //    }
  //    else r = 0.0
  //    r
  //  }

//  def apply[M <: Int, N <: Int](mtrx: Mat[M, N])(using ValueOf[M], ValueOf[N], M >= N =:= true): SV[M, N] = {
  def apply(rows:Int, columns:Int, mtrx: NArray[Double]): (NArray[Double], NArray[Double], NArray[Double]) = {

    // Derived from LINPACK code.
    // Initialize.
    val A: NArray[Double] = NArray.copy[Double](mtrx)

    val minDim: Int = Math.min(rows, columns)

    val s: NArray[Double] = new NArray[Double](columns) //Vec.zeros[N]
    val U: NArray[Double] = new NArray[Double](rows * columns) //Mat[M, N] = Mat.zeros[M, N]
    val V: NArray[Double] = new NArray[Double](columns * columns) //Mat[N, N] = Mat.zeros[N, N]
    val e: NArray[Double] = new NArray[Double](rows)

    val work: NArray[Double] = new NArray[Double](rows)

    // Reduce A to bidiagonal form, storing the diagonal elements
    // in s and the super-diagonal elements in e.

    val nct = Math.min(rows - 1, columns)
    val nrt = Math.max(0, Math.min(columns - 2, rows))

    var k0 = 0;
    while (k0 < Math.max(nct, nrt)) {

      if (k0 < nct) {

        // Compute the transformation for the k-th column and place the k-th diagonal in s[k].
        // Compute 2-norm of k-th column without under/overflow.

        s(k0) = 0.0
        var i: Int = k0;
        while (i < rows) {
          s(k0) = hypot(s(k0), A(lindex(i, k0, columns)))
          i += 1
        }
        if (s(k0) != 0.0) {
          if (A(lindex(k0, k0, columns)) < 0.0) {
            s(k0) = -s(k0)
          }
          var i0 = k0;
          while (i0 < rows) {
            A(lindex(i0, k0, columns)) /= s(k0)
            i0 += 1
          }
          A(lindex(k0, k0, columns)) += 1.0
        }
        s(k0) = -s(k0)
      }

      var j0: Int = k0 + 1;
      while (j0 < columns) {
        if ((k0 < nct) && (s(k0) != 0.0)) {
          // Apply the transformation.
          var t = 0.0
          var i: Int = k0;
          while (i < rows) {
            t += A(lindex(i, k0, columns)) * A(lindex(i, j0, columns))
            i += 1
          }
          t = -t / A(lindex(k0, k0, columns))
          i = k0;
          while (i < rows) {
            A(lindex(i, j0, columns)) += t * A(lindex(i, k0, columns))
            i += 1
          }
        }

        // Place the k-th row of A into e for the
        // subsequent calculation of the row transformation.

        e(j0) = A(lindex(k0, j0, columns))

        j0 += 1
      }
      if (k0 < nct) { // Place the transformation in U for subsequent back multiplication.
        var i: Int = k0;
        while (i < rows) {
          U(lindex(i, k0, columns)) = A(lindex(i, k0, columns))
          i += 1
        }
      }

      if (k0 < nrt) {
        // Compute the k-th row transformation and place the k-th super-diagonal in e[k].
        // Compute 2-norm without under/overflow.
        e(k0) = 0.0
        var i: Int = k0 + 1;
        while (i < columns) {
          e(k0) = hypot(e(k0), e(i))
          i += 1
        }

        if (e(k0) != 0.0) {
          if (e(k0 + 1) < 0.0) {
            e(k0) = -e(k0)
          }
          var i0: Int = k0 + 1;
          while (i0 < columns) {
            e(i0) = e(i0) / e(k0)
            i0 += 1
          }
          e(k0 + 1) = e(k0 + 1) + 1.0
        }
        e(k0) = -e(k0)
        if ((k0 + 1 < rows) && (e(k0) != 0.0)) {

          // Apply the transformation.

          var i1: Int = k0 + 1;
          while (i1 < rows) {
            work(i1) = 0.0
            i1 += 1
          }
          var j0: Int = k0 + 1;
          while (j0 < columns) {
            var i2: Int = k0 + 1;
            while (i2 < rows) {
              work(i2) = work(i2) + e(j0) * A(lindex(i2, j0, columns))
              i2 += 1
            }
            j0 += 1
          }
          j0 = k0 + 1;
          while (j0 < columns) {
            val t = -e(j0) / e(k0 + 1)
            var i2: Int = k0 + 1;
            while (i2 < rows) {
              A(lindex(i2, j0, columns)) += t * work(i2)
              i2 += 1
            }
            j0 += 1
          }
        }

        // Place the transformation in V for subsequent back multiplication.

        i = k0 + 1;
        while (i < columns) { // recycling i
          V(lindex(i, k0, columns)) = e(i)
          i += 1
        }

      }
      k0 += 1
    }

    // Set up the final bidiagonal matrix or order p.

    var p = Math.min(columns, rows + 1)
    if (nct < columns) {
      s(nct) = A(lindex(nct, nct, columns))
    }
    if (rows < p) {
      s(p - 1) = 0.0
    }
    if (nrt + 1 < p) {
      e(nrt) = A(lindex(nrt, p - 1, columns))
    }

    e(p - 1) = 0.0

    // generate U.

    var j: Int = nct;
    while (j < minDim) {
      var i: Int = 0;
      while (i < rows) {
        U(lindex(i, j, columns)) = 0.0
        i += 1
      }
      U(lindex(j, j, columns)) = 1.0
      j += 1
    }

    var k: Int = nct - 1;
    while (k > -1) {
      if (s(k) != 0.0) {
        var j0: Int = k + 1;
        while (j0 < minDim) {
          var t = 0.0
          var i: Int = k;
          while (i < rows) {
            t += U(lindex(i, k, columns)) * U(lindex(i, j0, columns))
            i += 1
          }
          t = -t / U(lindex(k, k, columns))
          i = k;
          while (i < rows) { // recycling i
            U(lindex(i, j0, columns)) += t * U(lindex(i, k, columns))
            i += 1
          }
          j0 += 1
        }
        var i: Int = k;
        while (i < rows) {
          U(lindex(i, k, columns)) = -U(lindex(i, k, columns))
          i += 1
        }
        U(lindex(k, k, columns)) = 1.0 + U(lindex(k, k, columns))
        i = 0;
        while (i < k - 1) { // recycling i
          U(lindex(i, k, columns)) = 0.0
          i += 1
        }
      } else {
        var i: Int = 0;
        while (i < rows) {
          U(lindex(i, k, columns)) = 0.0
          i += 1
        }
        U(lindex(k, k, columns)) = 1.0
      }
      k -= 1
    }

    // generate V.
    k = columns - 1;
    while (k > -1) { // recycling k
      if ((k < nrt) && (e(k) != 0.0)) {
        var j: Int = k + 1;
        while (j < minDim) {
          var t = 0.0
          var i: Int = k + 1;
          while (i < columns) {
            t += V(lindex(i, k, columns)) * V(lindex(i, j, columns))
            i += 1
          }
          t = -t / V(lindex(k + 1, k, columns))
          i = k + 1;
          while (i < columns) { // recycling i
            V(lindex(i, j, columns)) += t * V(lindex(i, k, columns))
            i += 1
          }
          j += 1
        }
      }
      var i: Int = 0;
      while (i < columns) {
        V(lindex(i, k, columns)) = 0.0
        i += 1
      }
      V(lindex(k, k, columns)) = 1.0
      k -= 1
    }

    // Main iteration loop for the singular values.

    val pp = p - 1
    var iter = 0
    val eps = Math.pow(2.0, -52.0)
    val tiny = Math.pow(2.0, -966.0)
    while (p > 0) {

      // Here is where a test for too many iterations would go.
      // This section of the program inspects for
      // negligible elements in the s and e arrays.  On
      // completion the variables kase and k are set as follows.
      // kase = 1     if s(p) and e[k-1] are negligible and k<p
      // kase = 2     if s(k) is negligible and k<p
      // kase = 3     if e[k-1] is negligible, k<p, and
      //              s(k), ..., s(p) are not negligible (qr step).
      // kase = 4     if e(p-1) is negligible (convergence).

      var kase = 0
      var k = p - 2
      var continue = k >= -1

      while (continue) {
        if (k == -1) {
          continue = false
        } else {
          if (Math.abs(e(k)) <= tiny + eps * (Math.abs(s(k)) + Math.abs(s(k + 1)))) {
            e(k) = 0.0
            continue = false
          } else {
            k -= 1
          }
        }
        continue = continue && k >= -1
      }

      if (k == p - 2) {
        kase = 4
      } else {
        var ks = p - 1
        continue = ks >= k
        while (continue) {
          if (ks == k) {
            continue = false
          } else {
            val t = (if (ks != p) Math.abs(e(ks)) else 0.0) +
              (if (ks != k + 1) Math.abs(e(ks - 1)) else 0.0)

            if (Math.abs(s(ks)) <= tiny + eps * t) {
              s(ks) = 0.0
              continue = false
            } else {
              ks -= 1
            }
          }
          continue = continue && ks >= k
        }
        if (ks == k) kase = 3
        else if (ks == p - 1) kase = 1
        else {
          kase = 2
          k = ks
        }
      }
      k += 1

      // Perform the task indicated by kase.

      kase match { // Deflate negligible s(p).
        case 1 =>
          var f = e(p - 2)
          e(p - 2) = 0.0
          var j: Int = p - 2;
          while (j <= k) {
            var t = hypot(s(j), f)
            val cs = s(j) / t
            val sn = f / t
            s(j) = t
            if (j != k) {
              f = -sn * e(j - 1)
              e(j - 1) = cs * e(j - 1)
            }
            var i: Int = 0;
            while (i < columns) {
              t = cs * V(lindex(i, j, columns)) + sn * V(lindex(i, p - 1, columns))
              V(lindex(i, p - 1, columns)) = -sn * V(lindex(i, j, columns)) + cs * V(lindex(i, p - 1, columns))
              V(lindex(i, j, columns)) = t
              i += 1
            }
            j -= 1
          }


        // Split at negligible s(k).
        case 2 =>
          var f = e(k - 1)
          e(k - 1) = 0.0
          var j: Int = k;
          while (j < p) {
            var t = hypot(s(j), f)
            val cs = s(j) / t
            val sn = f / t
            s(j) = t
            f = -sn * e(j)
            e(j) = cs * e(j)
            var i: Int = 0;
            while (i < rows) {
              t = cs * U(lindex(i, j, columns)) + sn * U(lindex(i, k - 1, columns))
              U(lindex(i, k - 1, columns)) = -sn * U(lindex(i, j, columns)) + cs * U(lindex(i, k - 1, columns))
              U(lindex(i, j, columns)) = t
              i += 1
            }
            j += 1
          }


        // Perform one qr step.
        case 3 =>
          // Calculate the shift.
          val scale = Math.max(Math.max(Math.max(Math.max(
            Math.abs(s(p - 1)), Math.abs(s(p - 2))), Math.abs(e(p - 2))),
            Math.abs(s(k))), Math.abs(e(k)))
          val sp = s(p - 1) / scale
          val spm1 = s(p - 2) / scale
          val epm1 = e(p - 2) / scale
          val sk = s(k) / scale
          val ek = e(k) / scale
          val b = ((spm1 + sp) * (spm1 - sp) + epm1 * epm1) / 2.0
          val c = (sp * epm1) * (sp * epm1)
          var shift = 0.0
          if ((b != 0.0) | (c != 0.0)) {
            shift = Math.sqrt(b * b + c)
            if (b < 0.0) {
              shift = -shift
            }
            shift = c / (b + shift)
          }
          var f = (sk + sp) * (sk - sp) + shift
          var g = sk * ek

          // Chase zeros.

          var j: Int = k;
          while (j < p - 1) {
            var t = hypot(f, g)
            var cs = f / t
            var sn = g / t
            if (j != k) {
              e(j - 1) = t
            }
            f = cs * s(j) + sn * e(j)
            e(j) = cs * e(j) - sn * s(j)
            g = sn * s(j + 1)
            s(j + 1) = cs * s(j + 1)
            var i: Int = 0;
            while (i < columns) {
              t = cs * V(lindex(i, j, columns)) + sn * V(lindex(i, j + 1, columns))
              V(lindex(i, j + 1, columns)) = -sn * V(lindex(i, j, columns)) + cs * V(lindex(i, j + 1, columns))
              V(lindex(i, j, columns)) = t
              i += 1
            }
            t = hypot(f, g)
            cs = f / t
            sn = g / t
            s(j) = t
            f = cs * e(j) + sn * s(j + 1)
            s(j + 1) = -sn * e(j) + cs * s(j + 1)
            g = sn * e(j + 1)
            e(j + 1) = cs * e(j + 1)
            if (j < rows - 1) {
              var i: Int = 0;
              while (i < rows) {
                t = cs * U(lindex(i, j, columns)) + sn * U(lindex(i, j + 1, columns))
                U(lindex(i, j + 1, columns)) = -sn * U(lindex(i, j, columns)) + cs * U(lindex(i, j + 1, columns))
                U(lindex(i, j, columns)) = t
                i += 1
              }
            }
            j += 1
          }
          e(p - 2) = f
          iter = iter + 1


        // Convergence.
        case 4 =>
          // Make the singular values positive.
          if (s(k) <= 0.0) {
            s(k) = if (s(k) < 0.0) -s(k) else 0.0
            var i: Int = 0;
            while (i <= pp) {
              V(lindex(i, k, columns)) = -V(lindex(i, k, columns))
              i += 1
            }
          }

          continue = k < pp
          // Order the singular values.
          while (continue) {
            if (s(k) >= s(k + 1)) {
              continue = false
            } else {
              var t = s(k)
              s(k) = s(k + 1)
              s(k + 1) = t
              if (k < columns - 1) {
                var i: Int = 0;
                while (i < columns) {
                  t = V(lindex(i, k + 1, columns))
                  V(lindex(i, k + 1, columns)) = V(lindex(i, k, columns))
                  V(lindex(i, k, columns)) = t
                  i += 1
                }
              }
              if (k < rows - 1) {
                var i: Int = 0;
                while (i < rows) {
                  t = U(lindex(i, k + 1, columns))
                  U(lindex(i, k + 1, columns)) = U(lindex(i, k, columns))
                  U(lindex(i, k, columns)) = t
                  i += 1
                }
              }
              k += 1
            }
            continue = continue && k < pp
          }
          iter = 0
          p -= 1
      }
    }
    //new SV[M, N](U, V, s)
    (U, V, s)
  }


  /** Effective numerical matrix rank
   *
   * @return Number of nonnegligible singular values.
   */
  def rank(m:Int, n:Int, singularValues:NArray[Double]): Int = {
    val eps = Math.pow(2.0, -52.0)
    val tol = Math.max(m, n) * singularValues(0) * eps
    var r = 0
    var i: Int = 0;
    while (i < singularValues.length) {
      if (singularValues(i) > tol) {
        r += 1
      }
      i += 1
    }
    r
  }

  def sInverse(singularValues:NArray[Double]):NArray[Double] = {
    val out = NArray.copy[Double](singularValues)
    var i:Int = 0
    while (i < out.length) {
      out(i) = 1.0 / out(i)
      i = i + 1
    }
    slash.matrix.util.diagonal(out)
  }
}

object SV {

  def apply[M <: Int, N <: Int](mtrx:Mat[M, N])(using ValueOf[M], ValueOf[N], M >= N =:= true):SV[M, N] = {
    val temp: (NArray[Double], NArray[Double], NArray[Double]) = SVSolver(valueOf[M], valueOf[N], mtrx.values)
    //new SV[M, N](U, V, s)
    new SV[M, N](new Mat[M,N](temp._1), new Mat[N,N](temp._2), Vec[N](temp._3))
  }
}

  /** Singular Value Decomposition.
  * <P>
  * For an rows-by-columns matrix A with rows >= columns, the singular value decomposition is
  * an rows-by-columns orthogonal matrix U, an columns-by-columns diagonal matrix S, and
  * an columns-by-columns orthogonal matrix V so that A = U*S*V'.
  * <P>n
  * The singular values, sigma[k] = S[k][k], are ordered so that
  * sigma[0] >= sigma[1] >= ... >= sigma[columns-1].
  * <P>
  * The singular value decompostion always exists, so the constructor will
  * never fail.  The matrix condition number and the effective numerical
  * rank can be computed from this decomposition.
  */

// Derived from LINPACK code.

/**
 * Construct the singular value decomposition
 * Structure to access U, S and V.
 *
 * @param U
 * @param V
 * @param singularValues
 * @param x$4
 * @param x$5
 * @param x$6
 * @tparam M
 * @tparam N
 */

class SV[M <: Int, N <: Int] private(
  val U:Mat[M, N], val V:Mat[N, N], val singularValues:Vec[N]
)(using ValueOf[M], ValueOf[N], M >= N =:= true) {
  val m:Int = valueOf[M]
  val n:Int = valueOf[N]

  /** Return the diagonal matrix of singular values
    *
    * @return S
    */
  lazy val S: Mat[N, N] = Mat.diagonal[N](singularValues)

  /** Return the diagonal matrix of singular values
   *
   * https://en.wikipedia.org/wiki/Singular_value_decomposition#Pseudoinverse
   * "where Σ† is the pseudoinverse of Σ, which is formed by replacing every non-zero diagonal entry by its reciprocal and transposing the resulting matrix."
   *
   * @return S
   */
  lazy val S_inverse: Mat[N,N] = Mat[N,N](SVSolver.sInverse(singularValues.asNativeArray))

  /** Two norm
    *
    * @return max(S)
    */
  lazy val norm2: Double = singularValues(0)

  /** Two norm condition number
    *
    * @return max(S)/min(S)
    */
  lazy val cond: Double = singularValues(0) / singularValues(Math.min(m, n) - 1)

  /** Effective numerical matrix rank
    *
    * @return Number of nonnegligible singular values.
    */
  lazy val rank: Int = SVSolver.rank(m, n, singularValues.asNativeArray)
}


object RTSV {

//  def apply[M <: Int, N <: Int](mtrx:Mat[M, N])(using ValueOf[M], ValueOf[N], M >= N =:= true):SV[M, N] = {
  def apply(A:RTMat):RTSV = {
    if(A.rowDimension < A.columnDimension) throw new RuntimeException(
      s"Mat has fewer rows than columns: ${A.rowDimension} x ${A.columnDimension}."
    )

    val temp: (NArray[Double], NArray[Double], NArray[Double]) = SVSolver(A.rowDimension, A.columnDimension, A.values)
    //new SV[M, N](U, V, s)
    new RTSV(
      A.rowDimension,
      A.columnDimension,
      new RTMat(A.rowDimension, A.columnDimension, temp._1),
      new RTMat(A.columnDimension, A.columnDimension, temp._2),
      RTVec(temp._3))
  }
}

/** Singular Value Decomposition.
 * <P>
 * For an rows-by-columns matrix A with rows >= columns, the singular value decomposition is
 * an rows-by-columns orthogonal matrix U, an columns-by-columns diagonal matrix S, and
 * an columns-by-columns orthogonal matrix V so that A = U*S*V'.
 * <P>n
 * The singular values, sigma[k] = S[k][k], are ordered so that
 * sigma[0] >= sigma[1] >= ... >= sigma[columns-1].
 * <P>
 * The singular value decompostion always exists, so the constructor will
 * never fail.  The matrix condition number and the effective numerical
 * rank can be computed from this decomposition.
 */

// Derived from LINPACK code.

/**
 * Construct the singular value decomposition
 * Structure to access U, S and V.
 *
 * @param U
 * @param V
 * @param singularValues
 * @param x$4
 * @param x$5
 * @param x$6
 * @tparam M
 * @tparam N
 */

class RTSV private(
  val rows:Int, val columns:Int, val U:RTMat, val V:RTMat, val singularValues:RTVec
) {
  require(rows >= columns , s"Matrix must have at least as many rows as columns, but encountered: $rows x $columns")

  /** Return the diagonal matrix of singular values
   *
   * @return S
   */
  lazy val S: RTMat = RTMat.diagonal(singularValues.asInstanceOf[RTVec])

  /** Return the diagonal matrix of singular values
   *
   * https://en.wikipedia.org/wiki/Singular_value_decomposition#Pseudoinverse
   * "where Σ† is the pseudoinverse of Σ, which is formed by replacing every non-zero diagonal entry by its reciprocal and transposing the resulting matrix."
   *
   * @return S
   */
  lazy val S_inverse: RTMat = RTMat.diagonal(SVSolver.sInverse(singularValues.asNativeArray).asInstanceOf[RTVec])

  /** Two norm
   *
   * @return max(S)
   */
  lazy val norm2: Double = singularValues(0)

  /** Two norm condition number
   *
   * @return max(S)/min(S)
   */
  lazy val cond: Double = singularValues(0) / singularValues(Math.min(rows, columns) - 1)

  /** Effective numerical matrix rank
   *
   * @return Number of nonnegligible singular values.
   */
  lazy val rank: Int = SVSolver.rank(rows, columns, singularValues.asNativeArray)
}
