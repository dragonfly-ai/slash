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
import narr.*
import slash.matrix.util.lindex
import slash.{dimensionCheck, squareInPlace}

object LUSolver {

  def computeLU(m:Int, n:Int, values:NArray[Double]):(NArray[Double], NArray[Int], Double) = {

    val minDim:Int = Math.min(m, n)

    val lu:NArray[Double] = NArray.copy[Double](values)
    val piv:NArray[Int] = NArray.tabulate[Int](m)((i:Int) => i)

    var pivsign:Double = 1.0

    val LUcolj:NArray[Double] = NArray.fill[Double](m)(0.0)

    // Outer loop.

    var j:Int = 0; while (j < n) { // Make a copy of the j-th column to localize references.
      var i:Int = 0; while (i < m) {
        LUcolj(i) = lu(lindex(i, j, n))
        i += 1
      }
      // Apply previous transformations.
      i = 0; while (i < m) {  // recycling i
        // Most of the time is spent in the following dot product.
        val kmax = Math.min(i, j)
        var s = 0.0
        var k:Int = 0; while (k < kmax) {
          s += lu(lindex(i, k, n)) * LUcolj(k)
          k += 1
        }
        LUcolj(i) = LUcolj(i) - s
        lu(lindex(i, j, n)) = LUcolj(i)
        i += 1
      }
      // Find pivot and exchange if necessary.
      var p = j
      i = j + 1; while (i < m) {  // recycling i
        if (Math.abs(LUcolj(i)) > Math.abs(LUcolj(p))) p = i
        i += 1
      }
      if (p != j) {
        var k0:Int = 0; while (k0 < n) {
          val t = lu(lindex(p, k0, n))
          lu(lindex(p, k0, n)) = lu(lindex(j, k0, n))
          lu(lindex(j, k0, n)) = t
          k0 += 1
        }
        val k = piv(p)
        piv(p) = piv(j)
        piv(j) = k
        pivsign = -pivsign
      }
      //println(s"j = $j and minDim = $minDim")
      // Compute multipliers.
      if (j < minDim && lu(lindex(j, j, n)) != 0.0) {
        var i0:Int = j + 1; while (i0 < m) {
          lu(lindex(i0, j, n)) = lu(lindex(i0, j, n)) / lu(lindex(j, j, n))
          i0 += 1
        }
      }
      j += 1
    }
    //new LU[M, N]
    (lu, piv, pivsign)
  }

  def singular(n: Int, values:NArray[Double]): Boolean = {
    var j: Int = 0
    while (j < n) {
      if (values(lindex(j, j, n)) == 0.0) return true
      j += 1
    }
    false
  }

  def computeL(m:Int, n:Int, lu:NArray[Double]):NArray[Double] = {
    val out:NArray[Double] = new NArray[Double](m * n)
    var i:Int = 0
    var r:Int = 0; while (r < m) {
      var c: Int = 0; while (c < n) {
        out(i) = {
          if (r > c) lu(lindex(r, c, n))
          else if (r == c) 1.0
          else 0.0
        }
        i += 1
        c += 1
      }
      r += 1
    }
    //Mat[M, N](values)
    out
  }

  def computeU(n:Int, lu:NArray[Double]):NArray[Double] = {
    val out: NArray[Double] = new NArray[Double](squareInPlace(n))
    var i: Int = 0
    var r: Int = 0
    while (r < n) {
      var c: Int = 0
      while (c < n) {
        out(i) = if (r > c) 0.0 else lu(lindex(r, c, n))
        i += 1
        c += 1
      }
      r += 1
    }
    out
  }

  // assumes a square matrix.
  def determinant(pivsign:Double, n:Int, lu:NArray[Double]):Double = {
    var d:Double = pivsign

    var j:Int = 0
    while (j < n) {
      d *= lu(lindex(j, j, n))
      j += 1
    }

    d
  }

//  def solve[V <: Int](B: Mat[M, V])(using ValueOf[V]): Mat[N, V] = {
  def solve(columns:Int, lu:NArray[Double], bColumns:Int, B:NArray[Double], piv:NArray[Int]): NArray[Double] = {

    // Copy right hand side with pivoting
    val nx: Int = bColumns
    //val X: Mat[N, V] = B.subMatrix[N, V](piv, 0)
    val X: NArray[Double] = util.subMatrix(bColumns, B, bColumns, piv, 0)

    // Solve L*Y = B(piv,:)
    var k: Int = 0;
    while (k < columns) {
      var i: Int = k + 1;
      while (i < columns) {
        var j: Int = 0;
        while (j < nx) {
          X(lindex(i, j, bColumns)) = X(lindex(i, j, bColumns)) - (X(lindex(k, j, bColumns)) * lu(lindex(i, k, columns)))
          j += 1
        }
        i += 1
      }
      k += 1
    }
    // Solve U*X = Y;
    k = columns - 1;
    while (k > -1) { // recycling k
      var j: Int = 0;
      while (j < nx) {
        X(lindex(k, j, bColumns)) = X(lindex(k, j, bColumns)) / lu(lindex(k, k, columns))
        j += 1
      }
      var i: Int = 0;
      while (i < k) {
        var j1: Int = 0;
        while (j1 < nx) {
          X(lindex(i, j1, bColumns)) = X(lindex(i, j1, bColumns)) - (X(lindex(k, j1, bColumns)) * lu(lindex(i, k, columns)))
          j1 += 1
        }
        i += 1
      }
      k -= 1
    }
    X
  }
}

object LU {

  def apply[M <: Int, N <: Int](A:Mat[M, N])(using ValueOf[M], ValueOf[N]):LU[M, N] = {
    val temp = LUSolver.computeLU(A.rows, A.columns, A.values)
    new LU[M, N](new Mat[M,N](temp._1), temp._2, temp._3)
  }

}

/** LU Decomposition.
  * <P>
  * For an m-by-n matrix A with m >= n, the LU decomposition is an m-by-n
  * unit lower triangular matrix L, an n-by-n upper triangular matrix U,
  * and a permutation vector piv of length m so that A(piv,:) = L*U.
  * If m < n, then L is m-by-m and U is m-by-n.
  * <P>
  * The LU decompostion with pivoting always exists, even if the matrix is
  * singular, so the constructor will never fail.  The primary use of the
  * LU decomposition is in the solution of square systems of simultaneous
  * linear equations.  This will fail if isNonsingular() returns false.
  */

/**
 * LU Decomposition
 * Structure to access L, U and piv.
 *
 * @param LU
 * @param piv
 * @param pivsign
 * @param x$4
 * @param x$5
 * @tparam M
 * @tparam N
 */

class LU[M <: Int, N <: Int] private (
  val LU:Mat[M, N], piv:NArray[Int], pivsign:Double
)(using ValueOf[M], ValueOf[N]) {  // Use a "left-looking", dot-product, Crout/Doolittle algorithm.

  val m:Int = valueOf[M]
  val n:Int = valueOf[N]

  /** Is the matrix nonsingular?
    *
    * @return true if U, and hence A, is nonsingular.
    */
  lazy val singular: Boolean = LUSolver.singular(n, LU.values)

  /** Return lower triangular factor
    *
    * @return L
    */
  lazy val L: Mat[M, N] = new Mat[M,N](LUSolver.computeL(m, n, LU.values))

  /** Return upper triangular factor
    *
    * @return U
    */
  lazy val U: Mat[N, N] = new Mat[N,N](LUSolver.computeU(n, LU.values))

  /** Return pivot permutation vector
    *
    * @return a copy of the piv NArray.
    */
  def pivot: NArray[Int] = NArray.copy[Int](piv) //NArray.tabulate[Int](m)((i:Int) => piv(i))

  /** Return pivot permutation vector as a one-dimensional double array
    *
    * @return (double) piv
    */
  def doubleValuedPivot(): NArray[Double] = NArray.tabulate[Double](m)((i:Int) => piv(i).toDouble)

  /** Determinant
    *
    * @return det(A)
    * @throws IllegalArgumentException  Mat must be square
    */
  def determinant: Double = {
    if (m != n) throw new IllegalArgumentException("Mat must be square.")
    LUSolver.determinant(pivsign, n, LU.values)
  }

  /** Solve A*X = B
    *
    * @param  B A Mat with as many rows as A and as many columns as B.
    * @return X so that L*U*X = B(piv,:)
    * @throws IllegalArgumentException Mat row dimensions must agree.
    * @throws RuntimeException  Mat is singular.
    */
  def solve[V <: Int](B: Mat[M, V])(using ValueOf[V]): Mat[N, V] = {
    if ( this.singular ) throw new RuntimeException( "Mat is singular." )
    new Mat[N,V](
      LUSolver.solve(n, LU.values, valueOf[V], B.values, piv: NArray[Int])
    )
  }
}

object RTLU {

  def apply(A:RTMat):RTLU = {
    val temp = LUSolver.computeLU(A.rowDimension, A.columnDimension, A.values)
    new RTLU(new RTMat(A.rowDimension, A.columnDimension, temp._1), temp._2, temp._3)
  }

}

// Use a "left-looking", dot-product, Crout/Doolittle algorithm.
class RTLU private (val LU:RTMat, piv:NArray[Int], pivsign:Double) {

  val m:Int = LU.rowDimension
  val n:Int = LU.columnDimension

  /** Is the matrix nonsingular?
   *
   * @return true if U, and hence A, is nonsingular.
   */
  lazy val singular: Boolean = LUSolver.singular(n, LU.values)

  /** Return lower triangular factor
   *
   * @return L
   */
  lazy val L: RTMat = new RTMat(m, n, LUSolver.computeL(m, n, LU.values))

  /** Return upper triangular factor
   *
   * @return U
   */
  lazy val U: RTMat = new RTMat(n, n, LUSolver.computeU(n, LU.values))

  /** Return pivot permutation vector
   *
   * @return a copy of the piv NArray.
   */
  def pivot: NArray[Int] = NArray.copy[Int](piv) //NArray.tabulate[Int](m)((i:Int) => piv(i))


  /** Return pivot permutation vector as a one-dimensional double array
   *
   * @return (double) piv
   */
  def doubleValuedPivot(): NArray[Double] = NArray.tabulate[Double](m)((i:Int) => piv(i).toDouble)

  /** Determinant
   *
   * @return det(A)
   * @throws IllegalArgumentException  Mat must be square
   */
  def determinant: Double = {
    if (m != n) throw new IllegalArgumentException("Mat must be square.")
    LUSolver.determinant(pivsign, n, LU.values)
  }

  /** Solve A*X = B
   *
   * @param  B A Mat with as many rows as A and as many columns as B.
   * @return X so that L*U*X = B(piv,:)
   * @throws IllegalArgumentException Mat row dimensions must agree.
   * @throws RuntimeException  Mat is singular.
   */
  def solve(B: RTMat): RTMat = {
    if ( this.singular ) throw new RuntimeException( "Mat is singular." )
    dimensionCheck(m, B.rowDimension)
    new RTMat(
      n,
      B.columnDimension,
      LUSolver.solve(n, LU.values, B.columnDimension, B.values, piv: NArray[Int])
    )
  }
}
