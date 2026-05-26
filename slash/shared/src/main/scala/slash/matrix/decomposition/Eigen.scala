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
import slash.matrix.util.lindex
import slash.vector.runtime.RTVec

import scala.math.hypot

object EigenSolver {

  // Symmetric Householder reduction to tridiagonal form.

  //private def tred2[N <: Int](Q: Mat[N, N])(using ValueOf[N]): Eigen[N] = {

  def tred2(n:Int, Q: NArray[Double]): (NArray[Double], NArray[Double], NArray[Double]) = {
    //  This is derived from the Algol procedures tred2 by
    //  Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
    //  Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutine in EISPACK.

    val lambda: NArray[Double] = new NArray[Double](n)
    val lmbdi: NArray[Double] = new NArray[Double](n)

    var j0:Int = 0
    while (j0 < n) {
      lambda(j0) = Q(lindex(n - 1, j0, n))
      j0 += 1
    }

    // Householder reduction to tridiagonal form.

    var i:Int = n - 1
    while ( i > 0) { // Scale to avoid under/overflow.
      var scale = 0.0
      var h = 0.0
      var s:Int = 0
      while (s < i) {
        scale = scale + Math.abs(lambda(s))
        s += 1
      }

      if (scale == 0.0) {
        lmbdi(i) = lambda(i - 1)
        var j:Int = 0
        while (j < i) {
          lambda(j) = Q(lindex(i - 1, j, n))
          Q(lindex(i, j, n)) = 0.0
          Q(lindex(j, i, n)) = 0.0
          j += 1
        }
      } else { // Generate Householder vector.
        var k:Int = 0
        while (k < i) {
          lambda(k) = lambda(k) / scale
          h += lambda(k) * lambda(k)
          k += 1
        }
        var f = lambda(i - 1)
        var g = Math.sqrt(h)
        if (f > 0) g = -g
        lmbdi(i) = scale * g
        h = h - f * g
        lambda(i - 1) = f - g

        var j:Int = 0
        while (j < i) {
          lmbdi(j) = 0.0
          j += 1
        }

        // Apply similarity transformation to remaining columns.
        j = 0
        while (j < i) { // recycling j
          f = lambda(j)
          Q(lindex(j, i, n)) = f
          g = lmbdi(j) + Q(lindex(j, j, n)) * f
          k = j + 1
          while (k <= i - 1) { // recycling k
            g += Q(lindex(k, j, n)) * lambda(k)
            lmbdi(k) = lmbdi(k) + (Q(lindex(k, j, n)) * f)
            k += 1
          }
          lmbdi(j) = g
          j += 1
        }
        f = 0.0
        j = 0
        while (j < i) {  // recycling j
          lmbdi(j) = lmbdi(j) / h
          f += lmbdi(j) * lambda(j)
          j += 1
        }
        val hh = f / (h + h)
        j = 0
        while (j < i) {  // recycling j
          lmbdi(j) = lmbdi(j) - (hh * lambda(j))
          j += 1
        }
        j = 0
        while (j < i) {  // recycling j
          f = lambda(j)
          g = lmbdi(j)
          k = j
          while (k <= i - 1) { // recycling k
            Q(lindex(k, j, n)) = Q(lindex(k, j, n)) - (f * lmbdi(k) + g * lambda(k))
            k += 1
          }
          lambda(j) = Q(lindex(i - 1, j, n))
          Q(lindex(i, j, n)) = 0.0
          j += 1
        }
      }
      lambda(i) = h
      i -= 1
    }

    // Accumulate transformations.
    i = 0
    while (i < n - 1) { // recycling i
      Q(lindex(n - 1, i, n)) = Q(lindex(i, i, n))
      Q(lindex(i, i, n)) = 1.0
      val h = lambda(i + 1)
      if (h != 0.0) {
        var k:Int = 0
        while (k <= i) {
          lambda(k) = Q(lindex(k, i + 1, n)) / h
          i += 1
        }
        var j:Int = 0
        while (j <= i) {
          var g = 0.0
          k = 0
          while (k <= i) { // recycling k
            g += Q(lindex(k, i + 1, n)) * Q(lindex(k, j, n))
            k += 1
          }
          k = 0
          while (k <= i) { // recycling k
            Q(lindex(k, j, n)) = Q(lindex(k, j, n)) - (g * lambda(k))
            k += 1
          }
          j += 1
        }
      }
      var k:Int = 0
      while (k <= i) {
        Q(lindex(k, i + 1, n)) = 0.0
        k += 1
      }
      i += 1
    }
    var j:Int =0
    while (j < n) {
      lambda(j) = Q(lindex(n - 1, j, n))
      Q(lindex(n - 1, j, n)) = 0.0
      j += 1
    }
    Q(lindex(n - 1, n - 1, n)) = 1.0
    lmbdi(0) = 0.0

    //tql2[N](Q, lambda, lmbdi)
    tql2(Q, lambda, lmbdi)
  }

  // Symmetric tridiagonal QL algorithm.

//  private def tql2[N <: Int](Q: Mat[N, N], lambda: Vec[N], lmbdi: Vec[N])(using ValueOf[N]): Eigen[N] = {

  private def tql2(Q:NArray[Double], lambda:NArray[Double], lmbdi:NArray[Double]): (NArray[Double], NArray[Double], NArray[Double]) = {
    //  This is derived from the Algol procedures tql2, by
    //  Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
    //  Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutine in EISPACK.

    val n:Int = lambda.length

    var i0:Int = 1
    while (i0 < n) {
      lmbdi(i0 - 1) = lmbdi(i0)
      i0 += 1
    }
    lmbdi(n - 1) = 0.0

    var f = 0.0
    var tst1 = 0.0
    val eps = Math.pow(2.0, -52.0)

    var l:Int = 0
    while (l < n) { // Find small subdiagonal element
      tst1 = Math.max(tst1, Math.abs(lambda(l)) + Math.abs(lmbdi(l)))
      var m = l

      while (m < n && Math.abs(lmbdi(m)) > eps * tst1) m += 1

      // If m == l, d[l] is an eigenvalue,
      // otherwise, iterate.
      if (m > l) {
        var iter = 0
        var continue: Boolean = true
        while (continue) {
          iter = iter + 1 // (Could check iteration count here.)

          // Compute implicit shift
          var g = lambda(l)
          var p = (lambda(l + 1) - g) / (2.0 * lmbdi(l))
          var r = hypot(p, 1.0)
          if (p < 0) r = -r
          lambda(l) = lmbdi(l) / (p + r)
          lambda(l + 1) = lmbdi(l) * (p + r)
          val dl1 = lambda(l + 1)
          var h = g - lambda(l)
          var i:Int = l + 2
          while (i < n) {
            lambda(i) = lambda(i) - h
            i += 1
          }
          f = f + h
          // Implicit QL transformation.
          p = lambda(m)
          var c = 1.0
          var c2 = c
          var c3 = c
          val el1 = lmbdi(l + 1)
          var s = 0.0
          var s2 = 0.0
          i = m - 1
          while (i >= l) {
            c3 = c2
            c2 = c
            s2 = s
            g = c * lmbdi(i)
            h = c * p
            r = hypot(p, lmbdi(i))
            lmbdi(i + 1) = s * r
            s = lmbdi(i) / r
            c = p / r
            p = c * lambda(i) - s * g
            lambda(i + 1) = h + s * (c * g + s * lambda(i))
            // Accumulate transformation.
            var k:Int = 0
            while (k < n) {
              h = Q(lindex(k, i + 1, n))
              Q(lindex(k, i + 1, n)) = s * Q(lindex(k, i, n)) + c * h
              Q(lindex(k, i, n)) = c * Q(lindex(k, i, n)) - s * h
              k += 1
            }
            i -= 1
          }
          p = -s * s2 * c3 * el1 * lmbdi(l) / dl1
          lmbdi(l) = s * p
          lambda(l) = c * p
          // Check for convergence.
          continue = Math.abs(lmbdi(l)) > eps * tst1
        }
      }
      lambda(l) = lambda(l) + f
      lmbdi(l) = 0.0
      l += 1
    }

    // Sort eigenvalues and corresponding vectors.

    var i:Int = 0
    while (i < n - 1) {
      var k = i
      var p = lambda(i)
      var j:Int = i + 1
      while (j < n) {
        if (lambda(j) < p) {
          k = j
          p = lambda(j)
        }
        j += 1
      }
      if (k != i) {
        lambda(k) = lambda(i)
        lambda(i) = p
        j = 0
        while (j < n) { // recycling j
          p = Q(lindex(j, i, n))
          Q(lindex(j, i, n)) = Q(lindex(j, k, n))
          Q(lindex(j, k, n)) = p
          j += 1
        }
      }
      i += 1
    }

    //new Eigen[N](Q, lambda, lmbdi)
    (Q, lambda, lmbdi)
  }

//  private def orthes[N <: Int](Q: Mat[N, N])(using ValueOf[N]): Eigen[N] = {

  def orthes(n:Int, Q: NArray[Double]): (NArray[Double], NArray[Double], NArray[Double]) = {
    //  This is derived from the Algol procedures orthes and ortran,
    //  by Martin and Wilkinson, Handbook for Auto. Comp.,
    //  Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutines in EISPACK.

    val H: NArray[Double] = NArray.copy[Double](Q)

    val ort: NArray[Double] = new NArray[Double](n)

    val high = n - 1

    var m:Int = 1
    while (m < high) { // Scale column.
      var scale = 0.0
      var s:Int = m
      while (s <= high) {
        scale = scale + Math.abs(H(lindex(s, m - 1, n)))
        s += 1
      }
      if (scale != 0.0) { // Compute Householder transformation.
        var h = 0.0
        var i0:Int = high
        while (i0 >= m) {
          ort(i0) = H(lindex(i0, m - 1, n)) / scale
          h += ort(i0) * ort(i0)
          i0 -= 1
        }
        var g = Math.sqrt(h)
        if (ort(m) > 0) g = -g
        h = h - ort(m) * g
        ort(m) = ort(m) - g
        // Apply Householder similarity transformation
        // H = (I-u*u'/h)*H*(I-u*u')/h)
        var j0:Int = m
        while (j0 < n) {
          var f = 0.0
          var i:Int = high
          while (i >= m) {
            f += ort(i) * H(lindex(i, j0, n))
            i -= 1
          }
          f = f / h
          i = m
          while (i <= high) { // recycling i
            H(lindex(i, j0, n)) = H(lindex(i, j0, n)) - (f * ort(i))
            i += 1
          }
          j0 += 1
        }

        var i:Int = 0
        while (i <= high) {
          var f = 0.0
          var j:Int = high
          while (j >= m) {
            f += ort(j) * H(lindex(i, j, n))
            j -= 1
          }
          f = f / h
          j = m
          while (j <= high) { // recycling j
            H(lindex(i, j, n)) = H(lindex(i, j, n)) - (f * ort(j))
            j += 1
          }
          i += 1
        }
        ort(m) = scale * ort(m)
        H(lindex(m, m - 1, n)) = scale * g
      }
      m += 1
    }

    // Accumulate transformations (Algol's ortran).

    var i0:Int = 0
    while (i0 < n) {
      var j:Int = 0
      while (j < n) {
        Q(lindex(i0, j, n)) = if (i0 == j) 1.0 else 0.0
        j += 1
      }
      i0 += 1
    }

    var m0:Int = high - 1
    while (m0 > 0) {
      if (H(lindex(m0, m0 - 1, n)) != 0.0) {
        var i1:Int = m0 + 1
        while (i1 <= high) {
          ort(i1) = H(lindex(i1, m0 - 1, n))
          i1 += 1
        }
        var j:Int = m0
        while (j <= high) {
          var g = 0.0
          var i:Int = m0
          while (i <= high) {
            g += ort(i) * Q(lindex(i, j, n))
            i += 1
          }
          // Double division avoids possible underflow
          g = (g / ort(m0)) / H(lindex(m0, m0 - 1, n))
          i = m0
          while (i <= high) { // recycling i
            Q(lindex(i, j, n)) = Q(lindex(i, j, n)) + (g * ort(i))
            i += 1
          }
          j += 1
        }
      }
      m0 -= 1
    }

    // Reduce Hessenberg to real Schur form.
    //hqr2[N](Q, H)
    hqr2(n, Q, H)
  }


  // Nonsymmetric reduction from Hessenberg to real Schur form.

//  private def hqr2[N <: Int](Q: Mat[N, N], H: Mat[N, N])(using ValueOf[N]): Eigen[N] = {

  private def hqr2[N <: Int](nn:Int, Q: NArray[Double], H: NArray[Double]): (NArray[Double], NArray[Double], NArray[Double]) = {

    //  This is derived from the Algol procedure hqr2,
    //  by Martin and Wilkinson, Handbook for Auto. Comp.,
    //  Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutine in EISPACK.
    // Initialize

    val lambda: NArray[Double] = new NArray[Double](nn)
    val lmbdi: NArray[Double] = new NArray[Double](nn)

    var ni: Int = nn - 1
    val low: Int = 0
    val high: Int = nn - 1
    val eps: Double = Math.pow(2.0, -52.0)
    var exshift: Double = 0.0
    var p = 0.0
    var q = 0.0
    var r = 0.0
    var s = 0.0
    var z = 0.0
    var t = 0.0
    var w = 0.0
    var x = 0.0
    var y = 0.0

    def cdiv(xr: Double, xi: Double, yr: Double, yi: Double): (Double, Double) = {
      var r = 0.0
      var d = 0.0
      if (Math.abs(yr) > Math.abs(yi)) {
        r = yi / yr
        d = yr + r * yi
        (
          (xr + r * xi) / d, // temp._1 = ._1
          (xi - r * xr) / d // temp._2 = ._2
        )
      } else {
        r = yr / yi
        d = yi + r * yr
        (
          (r * xr + xi) / d, // temp._1 = ._1
          (r * xi - xr) / d // temp._2 = ._2
        )
      }
    }

    // Store roots isolated by balanc and compute matrix norm

    var norm = 0.0
    var i:Int = 0
    while (i < nn) {
      if (i < low | i > high) {
        lambda(i) = H(lindex(i, i, nn))
        lmbdi(i) = 0.0
      }
      var j:Int = Math.max(i - 1, 0)
      while (j < nn) {
        norm = norm + Math.abs(H(lindex(i, j, nn)))
        j += 1
      }
      i += 1
    }

    // Outer loop over eigenvalue index

    var iter = 0
    while (ni >= low) { // Look for single small sub-diagonal element

      var l = ni
      var continue: Boolean = true
      while (l > low && continue) {
        s = Math.abs(H(lindex(l - 1, l - 1, nn))) + Math.abs(H(lindex(l, l, nn)))
        if (s == 0.0) s = norm
        if (Math.abs(H(lindex(l, l - 1, nn))) < eps * s) continue = false
        else l -= 1
      }

      // Check for convergence
      // One root found
      if (l == ni) {
        H(lindex(ni, ni, nn)) = H(lindex(ni, ni, nn)) + exshift
        lambda(ni) = H(lindex(ni, ni, nn))
        lmbdi(ni) = 0.0
        ni -= 1
        iter = 0
        // Two roots found
      } else if (l == ni - 1) {
        w = H(lindex(ni, ni - 1, nn)) * H(lindex(ni - 1, ni, nn))
        p = (H(lindex(ni - 1, ni - 1, nn)) - H(lindex(ni, ni, nn))) / 2.0
        q = p * p + w
        z = Math.sqrt(Math.abs(q))
        H(lindex(ni, ni, nn)) = H(lindex(ni, ni, nn)) + exshift
        H(lindex(ni - 1, ni - 1, nn)) = H(lindex(ni - 1, ni - 1, nn)) + exshift
        x = H(lindex(ni, ni, nn))
        // Real pair
        if (q >= 0) {
          if (p >= 0) z = p + z
          else z = p - z
          lambda(ni - 1) = x + z
          lambda(ni) = lambda(ni - 1)
          if (z != 0.0) lambda(ni) = x - w / z
          lmbdi(ni - 1) = 0.0
          lmbdi(ni) = 0.0
          x = H(lindex(ni, ni - 1, nn))
          s = Math.abs(x) + Math.abs(z)
          p = x / s
          q = z / s
          r = Math.sqrt(p * p + q * q)
          p = p / r
          q = q / r
          // Row modification
          var j0:Int = ni - 1
          while (j0 < nn) {
            z = H(lindex(ni - 1, j0, nn))
            H(lindex(ni - 1, j0, nn)) = q * z + p * H(lindex(ni, j0, nn))
            H(lindex(ni, j0, nn)) = q * H(lindex(ni, j0, nn)) - p * z
            j0 += 1
          }
          // Column modification
          var i0:Int = 0
          while (i0 <= ni) {
            z = H(lindex(i0, ni - 1, nn))
            H(lindex(i0, ni - 1, nn)) = q * z + p * H(lindex(i0, ni, nn))
            H(lindex(i0, ni, nn)) = q * H(lindex(i0, ni, nn)) - p * z
            i0 += 1
          }
          // Accumulate transformations
          i0 = low
          while (i0 <= high) { // recycling i0
            z = Q(lindex(i0, ni - 1, nn))
            Q(lindex(i0, ni - 1, nn)) = q * z + p * Q(lindex(i0, ni, nn))
            Q(lindex(i0, ni, nn)) = q * Q(lindex(i0, ni, nn)) - p * z
            i0 += 1
          }
          // Complex pair
        } else {
          lambda(ni - 1) = x + p
          lambda(ni) = x + p
          lmbdi(ni - 1) = z
          lmbdi(ni) = -z
        }
        ni = ni - 2
        iter = 0
        // No convergence yet
      } else {
        // Form shift

        x = H(lindex(ni, ni, nn))
        y = 0.0
        w = 0.0
        if (l < ni) {
          y = H(lindex(ni - 1, ni - 1, nn))
          w = H(lindex(ni, ni - 1, nn)) * H(lindex(ni - 1, ni, nn))
        }
        // Wilkinson's original ad hoc shift
        if (iter == 10) {
          exshift += x
          var i:Int = low
          while (i <= ni) {
            H(lindex(i, i, nn)) = H(lindex(i, i, nn)) - x
            i += 1
          }
          s = Math.abs(H(lindex(ni, ni - 1, nn))) + Math.abs(H(lindex(ni - 1, ni - 2, nn)))
          y = 0.75 * s
          x = y
          w = -0.4375 * s * s
        }

        // MATLAB's new ad hoc shift

        if (iter == 30) {
          s = (y - x) / 2.0
          s = s * s + w
          if (s > 0) {
            s = Math.sqrt(s)
            if (y < x) s = -s
            s = x - w / ((y - x) / 2.0 + s)
            var i:Int = low
            while (i <= ni) {
              H(lindex(i, i, nn)) = H(lindex(i, i, nn)) - s
              i += 1
            }
            exshift += s
            w = 0.964
            y = w
            x = y
          }
        }
        iter = iter + 1 // (Could check iteration count here.)

        // Look for two consecutive small sub-diagonal elements
        var m: Int = ni - 2
        continue = true
        while (m >= l && continue) {
          z = H(lindex(m, m, nn))
          r = x - z
          s = y - z
          p = (r * s - w) / H(lindex(m + 1, m, nn)) + H(lindex(m, m + 1, nn))
          q = H(lindex(m + 1, m + 1, nn)) - z - r - s
          r = H(lindex(m + 2, m + 1, nn))
          s = Math.abs(p) + Math.abs(q) + Math.abs(r)
          p = p / s
          q = q / s
          r = r / s
          if (m == l || Math.abs(H(lindex(m, m - 1, nn))) * (Math.abs(q) + Math.abs(r)) < eps * (Math.abs(p) * (Math.abs(H(lindex(m - 1, m - 1, nn))) + Math.abs(z) + Math.abs(H(lindex(m + 1, m + 1, nn)))))) {
            continue = false
          } else m -= 1
        }

        var i:Int = m + 2
        while (i <= ni) {
          H(lindex(i, i - 2, nn)) = 0.0
          if (i > m + 2) {
            H(lindex(i, i - 3, nn)) = 0.0
          }
          i += 1
        }

        // Double QR step involving rows l:ni and columns m:ni

        var k:Int = m
        while (k < ni) {
          continue = true
          val notLast: Boolean = k != ni - 1
          if (k != m) {
            p = H(lindex(k, k - 1, nn))
            q = H(lindex(k + 1, k - 1, nn))
            r = if (notLast) H(lindex(k + 2, k - 1, nn)) else 0.0
            x = Math.abs(p) + Math.abs(q) + Math.abs(r)

            if (x == 0.0) {
              continue = false
            } else {
              p = p / x
              q = q / x
              r = r / x
            }
          }

          if (continue) {
            s = Math.sqrt(p * p + q * q + r * r)
            if (p < 0) {
              s = -s
            }
            if (s != 0) {
              if (k != m) {
                H(lindex(k, k - 1, nn)) = -s * x
              } else {
                if (l != m) {
                  H(lindex(k, k - 1, nn)) = -(H(lindex(k, k - 1, nn)))
                }
              }
              p = p + s
              x = p / s
              y = q / s
              z = r / s
              q = q / p
              r = r / p

              // Row modification
              var j:Int = k
              while (j < nn) {
                p = H(lindex(k, j, nn)) + q * H(lindex(k + 1, j, nn))
                if (notLast) {
                  p = p + r * H(lindex(k + 2, j, nn))
                  H(lindex(k + 2, j, nn)) = H(lindex(k + 2, j, nn)) - p * z
                }
                H(lindex(k, j, nn)) = H(lindex(k, j, nn)) - p * x
                H(lindex(k + 1, j, nn)) = H(lindex(k + 1, j, nn)) - p * y
                j += 1
              }

              // Column modification
              var i1:Int = 0
              while (i1 <= Math.min(ni, k + 3)) {
                p = x * H(lindex(i1, k, nn)) + y * H(lindex(i1, k + 1, nn))
                if (notLast) {
                  p = p + z * H(lindex(i1, k + 2, nn))
                  H(lindex(i1, k + 2, nn)) = H(lindex(i1, k + 2, nn)) - p * r
                }
                H(lindex(i1, k, nn)) = H(lindex(i1, k, nn)) - p
                H(lindex(i1, k + 1, nn)) = H(lindex(i1, k + 1, nn)) - p * q
                i1 += 1
              }

              // Accumulate transformations
              i1 = low
              while (i1 <= high) {
                p = x * Q(lindex(i1, k, nn)) + y * Q(lindex(i1, k + 1, nn))
                if (notLast) {
                  p = p + z * Q(lindex(i1, k + 2, nn))
                  Q(lindex(i1, k + 2, nn)) = Q(lindex(i1, k + 2, nn)) - p * r
                }
                Q(lindex(i1, k, nn)) = Q(lindex(i1, k, nn)) - p
                Q(lindex(i1, k + 1, nn)) = Q(lindex(i1, k + 1, nn)) - p * q
                i1 += 1
              }
            } // (s != 0)
          } // if (continue)
          k += 1
        } // k loop
      } // check convergence
    } // while (ni >= low)

    // Backsubstitute to find vectors of upper triangular form

//    if (norm == 0.0) return new Eigen[N](Q, lambda, lmbdi)

    if (norm == 0.0) return (Q, lambda, lmbdi)

    ni = nn - 1
    while (ni >= 0) {
      p = lambda(ni)
      q = lmbdi(ni)
      // Real vector
      if (q == 0) {
        var l = ni
        H(lindex(ni, ni, nn)) = 1.0
        var i0:Int = ni - 1
        while (i0 > -1) {
          w = H(lindex(i0, i0, nn)) - p
          r = 0.0
          var j:Int = l
          while (j <= ni) {
            r = r + H(lindex(i0, j, nn)) * H(lindex(j, ni, nn))
            j += 1
          }
          if (lmbdi(i0) < 0.0) {
            z = w
            s = r
          } else {
            l = i0
            if (lmbdi(i0) == 0.0) {
              if (w != 0.0) H(lindex(i0, ni, nn)) = -r / w
              else H(lindex(i0, ni, nn)) = -r / (eps * norm)
              // Solve real equations
            } else {
              x = H(lindex(i0, i0 + 1, nn))
              y = H(lindex(i0 + 1, i0, nn))
              q = (lambda(i0) - p) * (lambda(i0) - p) + lmbdi(i0) * lmbdi(i0)
              t = (x * s - z * r) / q
              H(lindex(i0, ni, nn)) = t
              if (Math.abs(x) > Math.abs(z)) H(lindex(i0 + 1, ni, nn)) = (-r - w * t) / x
              else H(lindex(i0 + 1, ni, nn)) = (-s - y * t) / z
            }
            // Overflow control
            t = Math.abs(H(lindex(i0, ni, nn)))
            if ((eps * t) * t > 1) {
              var j0:Int = i0
              while (j0 <= ni) {
                H(lindex(j0, ni, nn)) = H(lindex(j0, ni, nn)) / t
                j0 += 1
              }
            }
          }
          i0 -= 1
        }
        // Complex vector
      } else if (q < 0) {
        var l = ni - 1
        // Last vector component imaginary so matrix is triangular
        if (Math.abs(H(lindex(ni, ni - 1, nn))) > Math.abs(H(lindex(ni - 1, ni, nn)))) {
          H(lindex(ni - 1, ni - 1, nn)) = q / H(lindex(ni, ni - 1, nn))
          H(lindex(ni - 1, ni, nn)) = -(H(lindex(ni, ni, nn)) - p) / H(lindex(ni, ni - 1, nn))
        } else {
          val temp = cdiv(0.0, -H(lindex(ni - 1, ni, nn)), H(lindex(ni - 1, ni - 1, nn)) - p, q)
          H(lindex(ni - 1, ni - 1, nn)) = temp._1
          H(lindex(ni - 1, ni, nn)) = temp._2
        }

        H(lindex(ni, ni - 1, nn)) = 0.0
        H(lindex(ni, ni, nn)) = 1.0

        var i2:Int = ni - 2
        while (i2 > -1) {
          var ra = .0
          var sa = .0
          var vr = .0
          var vi = .0
          ra = 0.0
          sa = 0.0
          var j:Int = l
          while (j <= ni) {
            ra = ra + H(lindex(i2, j, nn)) * H(lindex(j, ni - 1, nn))
            sa = sa + H(lindex(i2, j, nn)) * H(lindex(j, ni, nn))
            j += 1
          }
          w = H(lindex(i2, i2, nn)) - p
          if (lmbdi(i2) < 0.0) {
            z = w
            r = ra
            s = sa
          } else {
            l = i2
            if (lmbdi(i2) == 0) {
              val temp = cdiv(-ra, -sa, w, q)
              H(lindex(i2, ni - 1, nn)) = temp._1
              H(lindex(i2, ni, nn)) = temp._2
            } else { // Solve complex equations
              x = H(lindex(i2, i2 + 1, nn))
              y = H(lindex(i2 + 1, i2, nn))
              vr = (lambda(i2) - p) * (lambda(i2) - p) + lmbdi(i2) * lmbdi(i2) - q * q
              vi = (lambda(i2) - p) * 2.0 * q
              if (vr == 0.0 & vi == 0.0) vr = eps * norm * (Math.abs(w) + Math.abs(q) + Math.abs(x) + Math.abs(y) + Math.abs(z))
              val temp = cdiv(x * r - z * ra + q * sa, x * s - z * sa - q * ra, vr, vi)
              H(lindex(i2, ni - 1, nn)) = temp._1
              H(lindex(i2, ni, nn)) = temp._2
              if (Math.abs(x) > (Math.abs(z) + Math.abs(q))) {
                H(lindex(i2 + 1, ni - 1, nn)) = (-ra - w * H(lindex(i2, ni - 1, nn)) + q * H(lindex(i2, ni, nn))) / x
                H(lindex(i2 + 1, ni, nn)) = (-sa - w * H(lindex(i2, ni, nn)) - q * H(lindex(i2, ni - 1, nn))) / x
              } else {
                val temp = cdiv(-r - y * H(lindex(i2, ni - 1, nn)), -s - y * H(lindex(i2, ni, nn)), z, q)
                H(lindex(i2 + 1, ni - 1, nn)) = temp._1
                H(lindex(i2 + 1, ni, nn)) = temp._2
              }
            }
            t = Math.max(Math.abs(H(lindex(i2, ni - 1, nn))), Math.abs(H(lindex(i2, ni, nn))))
            if ((eps * t) * t > 1) {
              var j7:Int = i2
              while (j7 <= ni) {
                H(lindex(j7, ni - 1, nn)) = H(lindex(j7, ni - 1, nn)) / t
                H(lindex(j7, ni, nn)) = H(lindex(j7, ni, nn)) / t
                j7 += 1
              }
            }
          }
          i2 -= 1
        }
      }

      ni -= 1
    }
    // Vectors of isolated roots
    var i9:Int = 0
    while (i9 < nn) {
      if (i9 < low | i9 > high) {
        var j5:Int = i9
        while (j5 < nn) {
          Q(lindex(i9, j5, nn)) = H(lindex(i9, j5, nn))
          j5 += 1
        }
      }
      i9 += 1
    }

    // Back transformation to get eigenvectors of original matrix

    var j8:Int = nn - 1
    while (j8 >= low) {
      var i7:Int = low
      while (i7 <= high) {
        z = 0.0
        var k:Int = low
        while (k <= Math.min(j8, high)) {
          z = z + Q(lindex(i7, k, nn)) * H(lindex(k, j8, nn))
          k += 1
        }
        Q(lindex(i7, j8, nn)) = z
        i7 += 1
      }
      j8 -= 1
    }

//    new Eigen[N](Q, lambda, lmbdi)
    (Q, lambda, lmbdi)
  }

  def symetric(n:Int, values:NArray[Double]): Boolean = {

    var isSymmetric:Boolean = true
    // verify symmetry.
    var r: Int = 0
    while (r < n) {
      var c: Int = r
      while (c < n) {
        if (isSymmetric) isSymmetric = values(lindex(r, c, n)) == values(lindex(c, r, n))
        c += 1
      }
      r += 1
    }
    isSymmetric
  }

  def blockDiagonalEigenvalueMatrix(lambda: NArray[Double], lmbdi: NArray[Double]): NArray[Double] = {
    val X = slash.matrix.util.diagonal(lambda)
    var i: Int = 0
    while (i < lambda.length) {
      //X(i, i) = lambda(i)
      if (lmbdi(i) > 0) X(lindex(i, i + 1, lambda.length)) = lmbdi(i)
      else if (lmbdi(i) < 0) X(lindex(i, i - 1, lambda.length)) = lmbdi(i)
      i += 1
    }
    X
  }
}

object Eigen {

  def apply[N <: Int](A:Mat[N, N])(using ValueOf[N]): Eigen[N] = {

    val n = valueOf[N]

    val q = NArray.copy[Double](A.values)

    val temp = {
      if (EigenSolver.symetric(n, A.values)) EigenSolver.tred2(n, q) // Tridiagonalize
      else EigenSolver.orthes(n, q) // Reduce to Hessenberg form
    }

    new Eigen[N](new Mat[N,N](temp._1), Vec[N](temp._2), Vec[N](temp._3))
  }

}

/** Eigenvalues and eigenvectors of a real matrix.
 * <P>
 * If A is symmetric, then A = V*D*V' where the eigenvalue matrix D is
 * diagonal and the eigenvector matrix V is orthogonal.
 *I.e. A = V.times(D.times(V.transpose())) and
 *V.times(V.transpose()) equals the identity matrix.
 * <P>
 * If A is not symmetric, then the eigenvalue matrix D is block diagonal
 * with the real eigenvalues in 1-by-1 blocks and any complex eigenvalues,
 * lambda + i*mu, in 2-by-2 blocks, [lambda, mu; -mu, lambda].  The
 * columns of V represent the eigenvectors in the sense that A*V = V*D,
 * i.e. A.times(V) equals V.times(D).  The matrix V may be badly
 * conditioned, or even singular, so the validity of the equation
 * A = V*D*inverse(V) depends upon V.cond().
 * */

class Eigen[N <: Int] private(val Q:Mat[N, N], val lambda:Vec[N], val lmbdi:Vec[N])(using ValueOf[N]) {

  val n:Int = valueOf[N]

  /** Return the real parts of the eigenvalues
   *
   * @return real(diag(D))
   */
  def realEigenvalues: Vec[N] = lambda

  /** Return the imaginary parts of the eigenvalues
   *
   * @return imag(diag(D))
   */
  def imaginaryEigenvalues: Vec[N] = lmbdi

  inline def D: Mat[N, N] = blockDiagonalEigenvalueMatrix

  /** Return the block diagonal eigenvalue matrix
   *
   * @return Λ
   */
  def blockDiagonalEigenvalueMatrix: Mat[N, N] = new Mat[N, N](
    EigenSolver.blockDiagonalEigenvalueMatrix(
      lambda.asNativeArray,
      lmbdi.asNativeArray
    )
  )

}

object RTEigen {

  def apply(A:RTMat): RTEigen = {
    slash.dimensionCheck(A.rowDimension, A.columnDimension)

    val n = A.columnDimension

    val q = NArray.copy[Double](A.values)

    val temp = {
      if (EigenSolver.symetric(n, A.values)) EigenSolver.tred2(n, q) // Tridiagonalize
      else EigenSolver.orthes(n, q) // Reduce to Hessenberg form
    }

    new RTEigen(new RTMat(n, n, temp._1), RTVec(temp._2), RTVec(temp._3))
  }

}

class RTEigen private(val Q:RTMat, val lambda:RTVec, val lmbdi:RTVec) {

  val n:Int = Q.rowDimension

  /** Return the real parts of the eigenvalues
   *
   * @return real(diag(D))
   */
  def realEigenvalues: RTVec = lambda

  /** Return the imaginary parts of the eigenvalues
   *
   * @return imag(diag(D))
   */
  def imaginaryEigenvalues: RTVec = lmbdi

  inline def D: RTMat = blockDiagonalEigenvalueMatrix

  /** Return the block diagonal eigenvalue matrix
   *
   * @return Λ
   */
  def blockDiagonalEigenvalueMatrix: RTMat = new RTMat(
    n,
    n,
    EigenSolver.blockDiagonalEigenvalueMatrix(
      lambda.asNativeArray,
      lmbdi.asNativeArray
    )
  )
}