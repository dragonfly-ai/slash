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
import ai.dragonfly.math.vector.Vec
import narr.*

import scala.math.hypot

object Eigen {

  // Symmetric Householder reduction to tridiagonal form.

  private def tred2[N <: Int](Q: Matrix[N, N])(using ValueOf[N]): Eigen[N] = {
    //  This is derived from the Algol procedures tred2 by
    //  Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
    //  Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutine in EISPACK.

    val n:Int = valueOf[N]

    val λ: Vec[N] = Vec.fill[N](0.0)
    val λi: Vec[N] = Vec.fill[N](0.0)

    var j0:Int = 0; while (j0 < n) {
      λ(j0) = Q(n - 1, j0)
      j0 += 1
    }

    // Householder reduction to tridiagonal form.

    var i:Int = n - 1; while ( i > 0) { // Scale to avoid under/overflow.
      var scale = 0.0
      var h = 0.0
      var s:Int = 0; while (s < i) {
        scale = scale + Math.abs(λ(s))
        s += 1
      }

      if (scale == 0.0) {
        λi(i) = λ(i - 1)
        var j:Int = 0; while (j < i) {
          λ(j) = Q(i - 1, j)
          Q(i, j) = 0.0
          Q(j, i) = 0.0
          j += 1
        }
      } else { // Generate Householder vector.
        var k:Int = 0; while (k < i) {
          λ(k) = λ(k) / scale
          h += λ(k) * λ(k)
          k += 1
        }
        var f = λ(i - 1)
        var g = Math.sqrt(h)
        if (f > 0) g = -g
        λi(i) = scale * g
        h = h - f * g
        λ(i - 1) = f - g

        var j:Int = 0; while (j < i) {
          λi(j) = 0.0
          j += 1
        }

        // Apply similarity transformation to remaining columns.
        j = 0; while (j < i) { // recycling j
          f = λ(j)
          Q(j, i) = f
          g = λi(j) + Q(j, j) * f
          k = j + 1; while (k <= i - 1) { // recycling k
            g += Q(k, j) * λ(k)
            λi(k) = λi(k) + (Q(k, j) * f)
            k += 1
          }
          λi(j) = g
          j += 1
        }
        f = 0.0
        j = 0; while (j < i) {  // recycling j
          λi(j) = λi(j) / h
          f += λi(j) * λ(j)
          j += 1
        }
        val hh = f / (h + h)
        j = 0; while (j < i) {  // recycling j
          λi(j) = λi(j) - (hh * λ(j))
          j += 1
        }
        j = 0; while (j < i) {  // recycling j
          f = λ(j)
          g = λi(j)
          k = j; while (k <= i - 1) { // recycling k
            Q(k, j) = Q(k, j) - (f * λi(k) + g * λ(k))
            k += 1
          }
          λ(j) = Q(i - 1, j)
          Q(i, j) = 0.0
          j += 1
        }
      }
      λ(i) = h
      i -= 1
    }

    // Accumulate transformations.
    i = 0; while (i < n - 1) { // recycling i
      Q(n - 1, i) = Q(i, i)
      Q(i, i) = 1.0
      val h = λ(i + 1)
      if (h != 0.0) {
        var k:Int = 0; while (k <= i) {
          λ(k) = Q(k, i + 1) / h
          i += 1
        }
        var j:Int = 0; while (j <= i) {
          var g = 0.0
          k = 0; while (k <= i) { // recycling k
            g += Q(k, i + 1) * Q(k, j)
            k += 1
          }
          k = 0; while (k <= i) { // recycling k
            Q(k, j) = Q(k, j) - (g * λ(k))
            k += 1
          }
          j += 1
        }
      }
      var k:Int = 0; while (k <= i) {
        Q(k, i + 1) = 0.0
        k += 1
      }
      i += 1
    }
    var j:Int =0; while (j < n) {
      λ(j) = Q(n - 1, j)
      Q(n - 1, j) = 0.0
      j += 1
    }
    Q(n - 1, n - 1) = 1.0
    λi(0) = 0.0

    tql2[N](Q, λ, λi)
  }

  // Symmetric tridiagonal QL algorithm.

  private def tql2[N <: Int](Q: Matrix[N, N], λ: Vec[N], λi: Vec[N])(using ValueOf[N]): Eigen[N] = {
    //  This is derived from the Algol procedures tql2, by
    //  Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
    //  Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutine in EISPACK.

    val n:Int = valueOf[N]

    var i0:Int = 1; while (i0 < n) {
      λi(i0 - 1) = λi(i0)
      i0 += 1
    }
    λi(n - 1) = 0.0

    var f = 0.0
    var tst1 = 0.0
    val eps = Math.pow(2.0, -52.0)

    var l:Int = 0; while (l < n) { // Find small subdiagonal element
      tst1 = Math.max(tst1, Math.abs(λ(l)) + Math.abs(λi(l)))
      var m = l

      while (m < n && Math.abs(λi(m)) > eps * tst1) m += 1

      // If m == l, d[l] is an eigenvalue,
      // otherwise, iterate.
      if (m > l) {
        var iter = 0
        var continue: Boolean = true
        while (continue) {
          iter = iter + 1 // (Could check iteration count here.)

          // Compute implicit shift
          var g = λ(l)
          var p = (λ(l + 1) - g) / (2.0 * λi(l))
          var r = hypot(p, 1.0)
          if (p < 0) r = -r
          λ(l) = λi(l) / (p + r)
          λ(l + 1) = λi(l) * (p + r)
          val dl1 = λ(l + 1)
          var h = g - λ(l)
          var i:Int = l + 2; while (i < n) {
            λ(i) = λ(i) - h
            i += 1
          }
          f = f + h
          // Implicit QL transformation.
          p = λ(m)
          var c = 1.0
          var c2 = c
          var c3 = c
          val el1 = λi(l + 1)
          var s = 0.0
          var s2 = 0.0
          i = m - 1; while (i >= l) {
            c3 = c2
            c2 = c
            s2 = s
            g = c * λi(i)
            h = c * p
            r = hypot(p, λi(i))
            λi(i + 1) = s * r
            s = λi(i) / r
            c = p / r
            p = c * λ(i) - s * g
            λ(i + 1) = h + s * (c * g + s * λ(i))
            // Accumulate transformation.
            var k:Int = 0; while (k < n) {
              h = Q(k, i + 1)
              Q(k, i + 1) = s * Q(k, i) + c * h
              Q(k, i) = c * Q(k, i) - s * h
              k += 1
            }
            i -= 1
          }
          p = -s * s2 * c3 * el1 * λi(l) / dl1
          λi(l) = s * p
          λ(l) = c * p
          // Check for convergence.
          continue = Math.abs(λi(l)) > eps * tst1
        }
      }
      λ(l) = λ(l) + f
      λi(l) = 0.0
      l += 1
    }

    // Sort eigenvalues and corresponding vectors.

    var i:Int = 0; while (i < n - 1) {
      var k = i
      var p = λ(i)
      var j:Int = i + 1; while (j < n) {
        if (λ(j) < p) {
          k = j
          p = λ(j)
        }
        j += 1
      }
      if (k != i) {
        λ(k) = λ(i)
        λ(i) = p
        j = 0; while (j < n) { // recycling j
          p = Q(j, i)
          Q(j, i) = Q(j, k)
          Q(j, k) = p
          j += 1
        }
      }
      i += 1
    }

    new Eigen[N](Q, λ, λi)
  }

  private def orthes[N <: Int](Q: Matrix[N, N])(using ValueOf[N]): Eigen[N] = {
    //  This is derived from the Algol procedures orthes and ortran,
    //  by Martin and Wilkinson, Handbook for Auto. Comp.,
    //  Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutines in EISPACK.

    val n:Int = valueOf[N]

    val H: Matrix[N, N] = Q.copy

    val ort: NArray[Double] = NArray.fill[Double](n)(0.0)

    val high = n - 1

    var m:Int = 1; while (m < high) { // Scale column.
      var scale = 0.0
      var s:Int = m; while (s <= high) {
        scale = scale + Math.abs(H(s, m - 1))
        s += 1
      }
      if (scale != 0.0) { // Compute Householder transformation.
        var h = 0.0
        var i0:Int = high; while (i0 >= m) {
          ort(i0) = H(i0, m - 1) / scale
          h += ort(i0) * ort(i0)
          i0 -= 1
        }
        var g = Math.sqrt(h)
        if (ort(m) > 0) g = -g
        h = h - ort(m) * g
        ort(m) = ort(m) - g
        // Apply Householder similarity transformation
        // H = (I-u*u'/h)*H*(I-u*u')/h)
        var j0:Int = m; while (j0 < n) {
          var f = 0.0
          var i:Int = high; while (i >= m) {
            f += ort(i) * H(i, j0)
            i -= 1
          }
          f = f / h
          i = m; while (i <= high) { // recycling i
            H(i, j0) = H(i, j0) - (f * ort(i))
            i += 1
          }
          j0 += 1
        }

        var i:Int = 0; while (i <= high) {
          var f = 0.0
          var j:Int = high; while (j >= m) {
            f += ort(j) * H(i, j)
            j -= 1
          }
          f = f / h
          j = m; while (j <= high) { // recycling j
            H(i, j) = H(i, j) - (f * ort(j))
            j += 1
          }
          i += 1
        }
        ort(m) = scale * ort(m)
        H(m, m - 1) = scale * g
      }
      m += 1
    }

    // Accumulate transformations (Algol's ortran).

    var i0:Int = 0; while (i0 < n) {
      var j:Int = 0; while (j < n) {
        Q(i0, j) = if (i0 == j) 1.0 else 0.0
        j += 1
      }
      i0 += 1
    }

    var m0:Int = high - 1; while (m0 > 0) {
      if (H(m0, m0 - 1) != 0.0) {
        var i1:Int = m0 + 1; while (i1 <= high) {
          ort(i1) = H(i1, m0 - 1)
          i1 += 1
        }
        var j:Int = m0; while (j <= high) {
          var g = 0.0
          var i:Int = m0; while (i <= high) {
            g += ort(i) * Q(i, j)
            i += 1
          }
          // Double division avoids possible underflow
          g = (g / ort(m0)) / H(m0, m0 - 1)
          i = m0; while (i <= high) { // recycling i
            Q(i, j) = Q(i, j) + (g * ort(i))
            i += 1
          }
          j += 1
        }
      }
      m0 -= 1
    }

    // Reduce Hessenberg to real Schur form.
    hqr2[N](Q, H)
  }


  // Nonsymmetric reduction from Hessenberg to real Schur form.

  private def hqr2[N <: Int](Q: Matrix[N, N], H: Matrix[N, N])(using ValueOf[N]): Eigen[N] = {
    val nn: Int = valueOf[N]

    //  This is derived from the Algol procedure hqr2,
    //  by Martin and Wilkinson, Handbook for Auto. Comp.,
    //  Vol.ii-Linear Algebra, and the corresponding
    //  Fortran subroutine in EISPACK.
    // Initialize

    val λ: Vec[N] = Vec.fill[N](0.0)
    val λi: Vec[N] = Vec.fill[N](0.0)

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
    var i:Int = 0; while (i < nn) {
      if (i < low | i > high) {
        λ(i) = H(i, i)
        λi(i) = 0.0
      }
      var j:Int = Math.max(i - 1, 0); while (j < nn) {
        norm = norm + Math.abs(H(i, j))
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
        s = Math.abs(H(l - 1, l - 1)) + Math.abs(H(l, l))
        if (s == 0.0) s = norm
        if (Math.abs(H(l, l - 1)) < eps * s) continue = false
        else l -= 1
      }

      // Check for convergence
      // One root found
      if (l == ni) {
        H(ni, ni) = H(ni, ni) + exshift
        λ(ni) = H(ni, ni)
        λi(ni) = 0.0
        ni -= 1
        iter = 0
        // Two roots found
      } else if (l == ni - 1) {
        w = H(ni, ni - 1) * H(ni - 1, ni)
        p = (H(ni - 1, ni - 1) - H(ni, ni)) / 2.0
        q = p * p + w
        z = Math.sqrt(Math.abs(q))
        H(ni, ni) = H(ni, ni) + exshift
        H(ni - 1, ni - 1) = H(ni - 1, ni - 1) + exshift
        x = H(ni, ni)
        // Real pair
        if (q >= 0) {
          if (p >= 0) z = p + z
          else z = p - z
          λ(ni - 1) = x + z
          λ(ni) = λ(ni - 1)
          if (z != 0.0) λ(ni) = x - w / z
          λi(ni - 1) = 0.0
          λi(ni) = 0.0
          x = H(ni, ni - 1)
          s = Math.abs(x) + Math.abs(z)
          p = x / s
          q = z / s
          r = Math.sqrt(p * p + q * q)
          p = p / r
          q = q / r
          // Row modification
          var j0:Int = ni - 1; while (j0 < nn) {
            z = H(ni - 1, j0)
            H(ni - 1, j0) = q * z + p * H(ni, j0)
            H(ni, j0) = q * H(ni, j0) - p * z
            j0 += 1
          }
          // Column modification
          var i0:Int = 0; while (i0 <= ni) {
            z = H(i0, ni - 1)
            H(i0, ni - 1) = q * z + p * H(i0, ni)
            H(i0, ni) = q * H(i0, ni) - p * z
            i0 += 1
          }
          // Accumulate transformations
          i0 = low; while (i0 <= high) { // recycling i0
            z = Q(i0, ni - 1)
            Q(i0, ni - 1) = q * z + p * Q(i0, ni)
            Q(i0, ni) = q * Q(i0, ni) - p * z
            i0 += 1
          }
          // Complex pair
        } else {
          λ(ni - 1) = x + p
          λ(ni) = x + p
          λi(ni - 1) = z
          λi(ni) = -z
        }
        ni = ni - 2
        iter = 0
        // No convergence yet
      } else {
        // Form shift

        x = H(ni, ni)
        y = 0.0
        w = 0.0
        if (l < ni) {
          y = H(ni - 1, ni - 1)
          w = H(ni, ni - 1) * H(ni - 1, ni)
        }
        // Wilkinson's original ad hoc shift
        if (iter == 10) {
          exshift += x
          var i:Int = low; while (i <= ni) {
            H(i, i) = H(i, i) - x
            i += 1
          }
          s = Math.abs(H(ni, ni - 1)) + Math.abs(H(ni - 1, ni - 2))
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
            var i:Int = low; while (i <= ni) {
              H(i, i) = H(i, i) - s
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
          z = H(m, m)
          r = x - z
          s = y - z
          p = (r * s - w) / H(m + 1, m) + H(m, m + 1)
          q = H(m + 1, m + 1) - z - r - s
          r = H(m + 2, m + 1)
          s = Math.abs(p) + Math.abs(q) + Math.abs(r)
          p = p / s
          q = q / s
          r = r / s
          if (m == l || Math.abs(H(m, m - 1)) * (Math.abs(q) + Math.abs(r)) < eps * (Math.abs(p) * (Math.abs(H(m - 1, m - 1)) + Math.abs(z) + Math.abs(H(m + 1, m + 1))))) {
            continue = false
          } else m -= 1
        }

        var i:Int = m + 2; while (i <= ni) {
          H(i, i - 2) = 0.0
          if (i > m + 2) {
            H(i, i - 3) = 0.0
          }
          i += 1
        }

        // Double QR step involving rows l:ni and columns m:ni

        var k:Int = m; while (k < ni) {
          continue = true
          val notLast: Boolean = k != ni - 1
          if (k != m) {
            p = H(k, k - 1)
            q = H(k + 1, k - 1)
            r = if (notLast) H(k + 2, k - 1) else 0.0
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
                H(k, k - 1) = -s * x
              } else {
                if (l != m) {
                  H(k, k - 1) = -(H(k, k - 1))
                }
              }
              p = p + s
              x = p / s
              y = q / s
              z = r / s
              q = q / p
              r = r / p

              // Row modification
              var j:Int = k; while (j < nn) {
                p = H(k, j) + q * H(k + 1, j)
                if (notLast) {
                  p = p + r * H(k + 2, j)
                  H(k + 2, j) = H(k + 2, j) - p * z
                }
                H(k, j) = H(k, j) - p * x
                H(k + 1, j) = H(k + 1, j) - p * y
                j += 1
              }

              // Column modification
              var i1:Int = 0; while (i1 <= Math.min(ni, k + 3)) {
                p = x * H(i1, k) + y * H(i1, k + 1)
                if (notLast) {
                  p = p + z * H(i1, k + 2)
                  H(i1, k + 2) = H(i1, k + 2) - p * r
                }
                H(i1, k) = H(i1, k) - p
                H(i1, k + 1) = H(i1, k + 1) - p * q
                i1 += 1
              }

              // Accumulate transformations
              i1 = low; while (i1 <= high) {
                p = x * Q(i1, k) + y * Q(i1, k + 1)
                if (notLast) {
                  p = p + z * Q(i1, k + 2)
                  Q(i1, k + 2) = Q(i1, k + 2) - p * r
                }
                Q(i1, k) = Q(i1, k) - p
                Q(i1, k + 1) = Q(i1, k + 1) - p * q
                i1 += 1
              }
            } // (s != 0)
          } // if (continue)
          k += 1
        } // k loop
      } // check convergence
    } // while (ni >= low)

    // Backsubstitute to find vectors of upper triangular form

    if (norm == 0.0) return new Eigen[N](Q, λ, λi)

    ni = nn - 1
    while (ni >= 0) {
      p = λ(ni)
      q = λi(ni)
      // Real vector
      if (q == 0) {
        var l = ni
        H(ni, ni) = 1.0
        var i0:Int = ni - 1; while (i0 > -1) {
          w = H(i0, i0) - p
          r = 0.0
          var j:Int = l; while (j <= ni) {
            r = r + H(i0, j) * H(j, ni)
            j += 1
          }
          if (λi(i0) < 0.0) {
            z = w
            s = r
          } else {
            l = i0
            if (λi(i0) == 0.0) {
              if (w != 0.0) H(i0, ni) = -r / w
              else H(i0, ni) = -r / (eps * norm)
              // Solve real equations
            } else {
              x = H(i0, i0 + 1)
              y = H(i0 + 1, i0)
              q = (λ(i0) - p) * (λ(i0) - p) + λi(i0) * λi(i0)
              t = (x * s - z * r) / q
              H(i0, ni) = t
              if (Math.abs(x) > Math.abs(z)) H(i0 + 1, ni) = (-r - w * t) / x
              else H(i0 + 1, ni) = (-s - y * t) / z
            }
            // Overflow control
            t = Math.abs(H(i0, ni))
            if ((eps * t) * t > 1) {
              var j0:Int = i0; while (j0 <= ni) {
                H(j0, ni) = H(j0, ni) / t
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
        if (Math.abs(H(ni, ni - 1)) > Math.abs(H(ni - 1, ni))) {
          H(ni - 1, ni - 1) = q / H(ni, ni - 1)
          H(ni - 1, ni) = -(H(ni, ni) - p) / H(ni, ni - 1)
        } else {
          val temp = cdiv(0.0, -H(ni - 1, ni), H(ni - 1, ni - 1) - p, q)
          H(ni - 1, ni - 1) = temp._1
          H(ni - 1, ni) = temp._2
        }

        H(ni, ni - 1) = 0.0
        H(ni, ni) = 1.0

        var i2:Int = ni - 2; while (i2 > -1) {
          var ra = .0
          var sa = .0
          var vr = .0
          var vi = .0
          ra = 0.0
          sa = 0.0
          var j:Int = l; while (j <= ni) {
            ra = ra + H(i2, j) * H(j, ni - 1)
            sa = sa + H(i2, j) * H(j, ni)
            j += 1
          }
          w = H(i2, i2) - p
          if (λi(i2) < 0.0) {
            z = w
            r = ra
            s = sa
          } else {
            l = i2
            if (λi(i2) == 0) {
              val temp = cdiv(-ra, -sa, w, q)
              H(i2, ni - 1) = temp._1
              H(i2, ni) = temp._2
            } else { // Solve complex equations
              x = H(i2, i2 + 1)
              y = H(i2 + 1, i2)
              vr = (λ(i2) - p) * (λ(i2) - p) + λi(i2) * λi(i2) - q * q
              vi = (λ(i2) - p) * 2.0 * q
              if (vr == 0.0 & vi == 0.0) vr = eps * norm * (Math.abs(w) + Math.abs(q) + Math.abs(x) + Math.abs(y) + Math.abs(z))
              val temp = cdiv(x * r - z * ra + q * sa, x * s - z * sa - q * ra, vr, vi)
              H(i2, ni - 1) = temp._1
              H(i2, ni) = temp._2
              if (Math.abs(x) > (Math.abs(z) + Math.abs(q))) {
                H(i2 + 1, ni - 1) = (-ra - w * H(i2, ni - 1) + q * H(i2, ni)) / x
                H(i2 + 1, ni) = (-sa - w * H(i2, ni) - q * H(i2, ni - 1)) / x
              } else {
                val temp = cdiv(-r - y * H(i2, ni - 1), -s - y * H(i2, ni), z, q)
                H(i2 + 1, ni - 1) = temp._1
                H(i2 + 1, ni) = temp._2
              }
            }
            t = Math.max(Math.abs(H(i2, ni - 1)), Math.abs(H(i2, ni)))
            if ((eps * t) * t > 1) {
              var j7:Int = i2; while (j7 <= ni) {
                H(j7, ni - 1) = H(j7, ni - 1) / t
                H(j7, ni) = H(j7, ni) / t
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
    var i9:Int = 0; while (i9 < nn) {
      if (i9 < low | i9 > high) {
        var j5:Int = i9; while (j5 < nn) {
          Q(i9, j5) = H(i9, j5)
          j5 += 1
        }
      }
      i9 += 1
    }

    // Back transformation to get eigenvectors of original matrix

    var j8:Int = nn - 1; while (j8 >= low) {
      var i7:Int = low; while (i7 <= high) {
        z = 0.0
        var k:Int = low; while (k <= Math.min(j8, high)) {
          z = z + Q(i7, k) * H(k, j8)
          k += 1
        }
        Q(i7, j8) = z
        i7 += 1
      }
      j8 -= 1
    }


    new Eigen[N](Q, λ, λi)
  }

  def apply[N <: Int](m:Matrix[N, N])(using ValueOf[N]): Eigen[N] = {

    val A = m.values
    val n = valueOf[N] //Arg.getColumnDimension()

    var isSymmetric = true

    val Q:Matrix[N, N] = Matrix[N, N](
      NArray.tabulate[NArray[Double]](n)(
        (row: Int) => NArray.tabulate[Double](n)(
          (col: Int) => {
            if (isSymmetric) isSymmetric = A(row)(col) == A(col)(row)
            A(row)(col)
          }
        )
      )
    )

    if (isSymmetric) tred2[N](Q)  // Tridiagonalize
    else orthes[N](Q)  // Reduce to Hessenberg form

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
 *i.e. A.times(V) equals V.times(D).  The matrix V may be badly
 * conditioned, or even singular, so the validity of the equation
 * A = V*D*inverse(V) depends upon V.cond().
 * */

/** Check for symmetry, then construct the eigenvalue decomposition
 * Structure to access D and V.
 *
 * @param Arg Square matrix
 */

class Eigen[N <: Int] private(val Q:Matrix[N, N], val λ:Vec[N], val λi:Vec[N])(using ValueOf[N]) {

  val n:Int = valueOf[N]

  /** Return the real parts of the eigenvalues
   *
   * @return real(diag(D))
   */
  def realEigenvalues: Vec[N] = λ

  /** Return the imaginary parts of the eigenvalues
   *
   * @return imag(diag(D))
   */
  def imaginaryEigenvalues: Vec[N] = λi

  inline def D: Matrix[N, N] = Λ
  /** Return the block diagonal eigenvalue matrix
   *
   * @return Λ
   */
  def Λ: Matrix[N, N] = {
    val X = Matrix.diagonal[N](λ)
    var i:Int = 0; while (i < n) {
      //X(i, i) = λ(i)
      if (λi(i) > 0) X(i, i + 1) = λi(i)
      else if (λi(i) < 0) X(i, i - 1) = λi(i)
      i += 1
    }
    X
  }
}
