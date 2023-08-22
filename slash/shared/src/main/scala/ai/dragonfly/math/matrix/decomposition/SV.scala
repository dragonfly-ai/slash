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

import ai.dragonfly.math.vector.*
import ai.dragonfly.math.vector.Vec.*
import ai.dragonfly.math.matrix.*
import narr.*
import scala.math.hypot
import scala.compiletime.ops.int.*
//import SV.hypot
object SV {

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

  def apply[M <: Int, N <: Int](mtrx:Matrix[M, N])(using ValueOf[M], ValueOf[N], M >= N =:= true):SV[M, N] = {

    // Derived from LINPACK code.
    // Initialize.
    val A: Matrix[M, N] = mtrx.copy

    val rows:Int = valueOf[M]
    val columns:Int = valueOf[N]

    val minDim:Int = Math.min(rows, columns);

    val s: Vec[N] = Vec.zeros[N]
    val U: Matrix[M, N] = Matrix.zeros[M, N]
    val V: Matrix[N, N] = Matrix.zeros[N, N]
    val e: Vec[M] = Vec.fill[M](0.0)
    val work = NArray.fill[Double](rows)(0.0)

    // Reduce A to bidiagonal form, storing the diagonal elements
    // in s and the super-diagonal elements in e.

    val nct = Math.min(rows - 1, columns)
    val nrt = Math.max(0, Math.min(columns - 2, rows))

    var k0 = 0; while (k0 < Math.max(nct, nrt)) {

      if (k0 < nct) {

        // Compute the transformation for the k-th column and place the k-th diagonal in s[k].
        // Compute 2-norm of k-th column without under/overflow.

        s(k0) = 0.0
        var i:Int = k0; while (i < rows) {
          s(k0) = hypot(s(k0), A(i, k0))
          i += 1
        }
        if (s(k0) != 0.0) {
          if (A(k0, k0) < 0.0) {
            s(k0) = -s(k0)
          }
          var i0 = k0; while (i0 < rows) {
            A(i0, k0) /= s(k0)
            i0 += 1
          }
          A(k0, k0) += 1.0
        }
        s(k0) = -s(k0)
      }

      var j0:Int = k0 + 1; while (j0 < columns) {
        if ((k0 < nct) && (s(k0) != 0.0)) {
          // Apply the transformation.
          var t = 0.0
          var i:Int = k0; while (i < rows) {
            t += A(i, k0) * A(i, j0)
            i += 1
          }
          t = -t / A(k0, k0)
          i = k0; while (i < rows) {
            A(i, j0) += t * A(i, k0)
            i += 1
          }
        }

        // Place the k-th row of A into e for the
        // subsequent calculation of the row transformation.

        e(j0) = A(k0, j0)

        j0 += 1
      }
      if (k0 < nct) { // Place the transformation in U for subsequent back multiplication.
        var i:Int = k0; while (i < rows) {
          U(i, k0) = A(i, k0)
          i += 1
        }
      }

      if (k0 < nrt) {
        // Compute the k-th row transformation and place the k-th super-diagonal in e[k].
        // Compute 2-norm without under/overflow.
        e(k0) = 0.0
        var i:Int = k0 + 1; while (i < columns) {
          e(k0) = hypot(e(k0), e(i))
          i += 1
        }

        if (e(k0) != 0.0) {
          if (e(k0 + 1) < 0.0) {
            e(k0) = -e(k0)
          }
          var i0:Int = k0 + 1; while (i0 < columns) {
            e(i0) = e(i0) / e(k0)
            i0 += 1
          }
          e(k0 + 1) = e(k0 + 1) + 1.0
        }
        e(k0) = -e(k0)
        if ((k0 + 1 < rows) && (e(k0) != 0.0)) {

          // Apply the transformation.

          var i1:Int = k0 + 1; while (i1 < rows) {
            work(i1) = 0.0
            i1 += 1
          }
          var j0:Int = k0 + 1; while (j0 < columns) {
            var i2:Int = k0 + 1; while (i2 < rows) {
              work(i2) += e(j0) * A(i2, j0)
              i2 += 1
            }
            j0 += 1
          }
          j0 = k0 + 1; while (j0 < columns) {
            val t = -e(j0) / e(k0 + 1)
            var i2:Int = k0 + 1; while (i2 < rows) {
              A(i2, j0) += t * work(i2)
              i2 += 1
            }
            j0 += 1
          }
        }

        // Place the transformation in V for subsequent back multiplication.

        i = k0 + 1; while (i < columns) { // recycling i
          V(i, k0) = e(i)
          i += 1
        }

      }
      k0 += 1
    }

    // Set up the final bidiagonal matrix or order p.

    var p = Math.min(columns, rows + 1)
    if (nct < columns) {
      s(nct) = A(nct, nct)
    }
    if (rows < p) {
      s(p - 1) = 0.0
    }
    if (nrt + 1 < p) {
      e(nrt) = A(nrt, p - 1)
    }

    e(p - 1) = 0.0

    // generate U.

    var j:Int = nct; while (j < minDim) {
      var i:Int = 0; while (i < rows) {
        U(i, j) = 0.0
        i += 1
      }
      U(j, j) = 1.0
      j += 1
    }

    var k:Int = nct - 1; while (k > -1) {
      if (s(k) != 0.0) {
        var j0:Int = k + 1; while  (j0 < minDim) {
          var t = 0.0
          var i:Int = k; while (i < rows) {
            t += U(i, k) * U(i, j0)
            i += 1
          }
          t = -t / U(k, k)
          i = k; while (i < rows) { // recycling i
            U(i, j0) += t * U(i, k)
            i += 1
          }
          j0 += 1
        }
        var i:Int = k; while (i < rows) {
          U(i, k) = -U(i, k)
          i += 1
        }
        U(k, k) = 1.0 + U(k, k)
        i = 0; while (i < k - 1) { // recycling i
          U(i, k) = 0.0
          i += 1
        }
      } else {
        var i:Int = 0; while (i < rows) {
          U(i, k) = 0.0
          i += 1
        }
        U(k, k) = 1.0
      }
      k -= 1
    }

    // generate V.
    k = columns - 1; while (k > -1) { // recycling k
      if ((k < nrt) && (e(k) != 0.0)) {
        var j:Int = k + 1; while (j < minDim) {
          var t = 0.0
          var i:Int = k + 1; while (i < columns) {
            t += V(i, k) * V(i, j)
            i += 1
          }
          t = -t / V(k + 1, k)
          i = k + 1; while (i < columns) { // recycling i
            V(i, j) += t * V(i, k)
            i += 1
          }
          j += 1
        }
      }
      var i:Int = 0; while (i < columns) {
        V(i, k) = 0.0
        i += 1
      }
      V(k, k) = 1.0
      k -= 1
    }

    // Main iteration loop for the singular values.

    val pp = p - 1
    var iter = 0
    val eps = Math.pow(2.0, -52.0)
    val tiny = Math.pow(2.0, -966.0)
    while ( p > 0 ) {

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

      while ( continue ) {
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
        while ( continue ) {
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
          var j:Int = p - 2; while (j <= k) {
            var t = hypot(s(j), f)
            val cs = s(j) / t
            val sn = f / t
            s(j) = t
            if (j != k) {
              f = -sn * e(j - 1)
              e(j - 1) = cs * e(j - 1)
            }
            var i:Int = 0; while (i < columns) {
              t = cs * V(i, j) + sn * V(i, p - 1)
              V(i, p - 1) = -sn * V(i, j) + cs * V(i, p - 1)
              V(i, j) = t
              i += 1
            }
            j -= 1
          }


        // Split at negligible s(k).
        case 2 =>
          var f = e(k - 1)
          e(k - 1) = 0.0
          var j:Int = k; while (j < p) {
            var t = hypot(s(j), f)
            val cs = s(j) / t
            val sn = f / t
            s(j) = t
            f = -sn * e(j)
            e(j) = cs * e(j)
            var i:Int = 0; while (i < rows) {
              t = cs * U(i, j) + sn * U(i, k - 1)
              U(i, k - 1) = -sn * U(i, j) + cs * U(i, k - 1)
              U(i, j) = t
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

          var j:Int = k; while (j < p - 1) {
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
            var i:Int = 0; while (i < columns) {
              t = cs * V(i, j) + sn * V(i, j + 1)
              V(i, j + 1) = -sn * V(i, j) + cs * V(i, j + 1)
              V(i, j) = t
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
              var i:Int = 0; while (i < rows) {
                t = cs * U(i, j) + sn * U(i, j + 1)
                U(i, j + 1) = -sn * U(i, j) + cs * U(i, j + 1)
                U(i, j) = t
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
            s(k) = (if (s(k) < 0.0) -s(k) else 0.0)
            var i:Int = 0; while (i <= pp) {
              V(i, k) = -V(i, k)
              i += 1
            }
          }

          continue = k < pp
          // Order the singular values.
          while ( continue) {
            if (s(k) >= s(k + 1)) {
              continue = false
            } else {
              var t = s(k)
              s(k) = s(k + 1)
              s(k + 1) = t
              if (k < columns - 1) {
                var i:Int = 0; while (i < columns) {
                  t = V(i, k + 1)
                  V(i, k + 1) = V(i, k)
                  V(i, k) = t
                  i += 1
                }
              }
              if (k < rows - 1) {
                var i:Int = 0; while (i < rows) {
                  t = U(i, k + 1)
                  U(i, k + 1) = U(i, k)
                  U(i, k) = t
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
    new SV[M, N](U, V, s)
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

/** Construct the singular value decomposition
 * Structure to access U, S and V.
 *
 * @param M Rectangular matrix
 */

class SV[M <: Int, N <: Int] private(
  val U:Matrix[M, N], val V:Matrix[N, N], val singularValues:Vec[N]
)(using ValueOf[M], ValueOf[N], M >= N =:= true) {
  val m:Int = valueOf[M]
  val n:Int = valueOf[N]

  /** Return the diagonal matrix of singular values
    *
    * @return S
    */
  inline def S: Matrix[N, N] = Matrix.diagonal[N](singularValues)

  /** Return the diagonal matrix of singular values
   *
   * https://en.wikipedia.org/wiki/Singular_value_decomposition#Pseudoinverse
   * "where Σ† is the pseudoinverse of Σ, which is formed by replacing every non-zero diagonal entry by its reciprocal and transposing the resulting matrix."
   *
   * @return S
   */
  inline def S_inverse: Matrix[N, N] = Matrix.diagonal[N](
    Vec.tabulate[N]( (i:Int) => 1.0 / singularValues(i) )
  )

  /** Two norm
    *
    * @return max(S)
    */
  def norm2: Double = singularValues(0)

  /** Two norm condition number
    *
    * @return max(S)/min(S)
    */
  def cond: Double = singularValues(0) / singularValues(Math.min(m, n) - 1)

  /** Effective numerical matrix rank
    *
    * @return Number of nonnegligible singular values.
    */
  def rank: Int = {
    val eps = Math.pow(2.0, -52.0)
    val tol = Math.max(m, n) * singularValues(0) * eps
    var r = 0
    var i:Int = 0; while (i < singularValues.dimension) {
      if (singularValues(i) > tol) {
        r += 1
      }
      i += 1
    }
    r
  }
}
