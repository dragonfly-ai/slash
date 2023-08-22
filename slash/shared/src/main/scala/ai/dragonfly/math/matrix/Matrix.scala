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

package ai.dragonfly.math.matrix

import ai.dragonfly.math.matrix.decomposition.{Cholesky, Eigen, LU, QR, SV}
import ai.dragonfly.math.matrix.util.CannotExpressMatrixAsVector
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.vector.Vec.*
import narr.*

import scala.compiletime.ops.int.*
import scala.math.hypot

/**
  * This library is fundamentally an adaptation of the Java Matrix library, JaMa, by MathWorks Inc. and the National Institute of Standards and Technology.
  */

object Matrix {
  /** Construct a matrix from a copy of a 2-D array.
   *
   * @param values Two-dimensional array of doubles.
   * @throws IllegalArgumentException All rows must have the same length
   */
  def constructWithCopy[M <: Int, N <: Int](values: NArray[NArray[Double]])(using ValueOf[M], ValueOf[N]): Matrix[M, N] = {

    dimensionCheck(valueOf[M], values.length)
    dimensionCheck(valueOf[N], values(0).length)

    new Matrix(
      NArray.tabulate[NArray[Double]](values.length)(
        (row: Int) => NArray.tabulate[Double](values(0).length)(
          (col: Int) => values(row)(col)
        )
      )
    )
  }

  /** Generate matrix with random elements
   * @param maxNorm optional Maximum random generated value allowed.
   * @param r optional random instance.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with uniformly distributed random elements.
   */
  def random[M <: Int, N <: Int](
    maxNorm:Double = 1.0,
    r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom
  )(using ValueOf[M], ValueOf[N]): Matrix[M, N] = new Matrix(
    NArray.tabulate[NArray[Double]](valueOf[M])(
      _ => NArray.tabulate[Double](valueOf[N])(
        _ => r.nextDouble()
      )
    )
  )

  /** Generate identity matrix
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with ones on the diagonal and zeros elsewhere.
   */
  def identity[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]): Matrix[M, N] = diagonal[M, N](1.0)

  /** Generate identity matrix scaled by value parameter.
   *
   * @param rows Number of rows.
   * @param columns Number of colums.
   * @param value scalar multiplier.
   * @return An MxN matrix with ones on the diagonal and zeros elsewhere.
   */
  def diagonal[M <: Int, N <: Int](value:Double)(using ValueOf[M], ValueOf[N]): Matrix[M, N] = {
    val out:Matrix[M, N] = zeros[M, N]
    val min:Int = Math.min(valueOf[M], valueOf[N])
    var i:Int = 0
    while (i < min) {
      out(i, i) = value
      i = i + 1
    }
    out
  }

  /**
   * Generate a square matrix with the supplied vector along the diagonal.
   * @param v a vector
   * @return
   */
  def diagonal[D <: Int](v:Vec[D])(using ValueOf[D]): Matrix[D, D] = {
    val out:Matrix[D, D] = zeros[D, D]
    var i:Int = 0
    while (i < v.dimension) {
      out(i, i) = v(i)
      i = i + 1
    }
    out
  }

  def diagonal[M <: Int, N <: Int, D <: Int](v: Vec[D])(using ValueOf[M], ValueOf[N], ValueOf[D]): Matrix[M, N] = {

    val rows:Int = valueOf[M]
    val columns:Int = valueOf[N]

    val out: Matrix[M, N] = zeros[M, N]

    var i: Int = 0
    while (i < Math.min(valueOf[D], Math.min(rows, columns))) {
      out(i, i) = v(i)
      i = i + 1
    }
    out
  }

  /** Construct a matrix from a 2-D array.
   *
   * @param values Two-dimensional array of doubles.
   * @throws IllegalArgumentException All rows must have the same length
   */

  def apply[M <: Int, N <: Int](values:NArray[NArray[Double]])(using ValueOf[M], ValueOf[N]):Matrix[M, N] = {
    dimensionCheck(valueOf[M], values.length)
    dimensionCheck(valueOf[N], values(0).length)
    new Matrix[M, N](values)
  }
//    val l:Int = values(0).length
//    for (r <- 1 until values.length) {
//      if (values(r).length != l) throw new IllegalArgumentException("Cannot create a Matrix from a Jagged Array.")
//    }
//    new Matrix(values)
//  }


  /** Construct an MxN constant matrix.
   * @param value Fill the matrix with this scalar value.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return an MxN constant matrix.
   */
  def apply[M <: Int, N <: Int](value: Double)(using ValueOf[M], ValueOf[N]):Matrix[M, N] = apply[M, N](
    NArray.tabulate[NArray[Double]](valueOf[M])(
      (_: Int) => NArray.fill[Double](valueOf[N])(value)
    )
  )

  /** Construct an MxN matrix of zeros.
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   */
  def zeros[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]):Matrix[M, N] = apply[M, N](0.0)


  /** Construct a matrix from a one-dimensional packed array
   *
   * @param vals One-dimensional array of doubles, packed by columns (ala Fortran).
   * @param m    Number of rows.
   * @throws IllegalArgumentException Array length must be a multiple of m.
   */
  def apply[M <: Int, N <: Int](vals: NArray[Double])(using ValueOf[M], ValueOf[N]):Matrix[M, N] = apply[M, N]({
    val m: Int = valueOf[M]
    val n: Int = vals.length / m
    dimensionCheck(n, valueOf[N])
    if (m < 1 || m * n != vals.length) {
      throw new IllegalArgumentException(s"Matrix(vals:NArray[Double], m:Int) : m = $m does not evenly divide vals.length = ${vals.length}.");
    }

    NArray.tabulate[NArray[Double]](m)(
      (i: Int) => NArray.tabulate[Double](n)(
        (j: Int) => vals(i + j * m)
      )
    )
  })

}

class Matrix[M <: Int, N <: Int] private(val values: NArray[NArray[Double]])(using ValueOf[M], ValueOf[N]) {

  val rows: Int = valueOf[M]
  val columns: Int = valueOf[N]

  /** Make a deep copy of a matrix
    */
  def copy: Matrix[M, N] = new Matrix[M, N](copyValues)

  /** Copy the internal two-dimensional array.
    *
    * @return Two-dimensional array copy of matrix elements.
    */
  def copyValues: NArray[NArray[Double]] = NArray.tabulate[NArray[Double]](rows)(
    (row:Int) => NArray.tabulate[Double](columns)(
      (col:Int) => values(row)(col)
    )
  )

  /** Make a one-dimensional column packed copy of the internal array.
    *
    * @return Matrix elements packed in a one-dimensional array by columns.
    */
  def columnPackedArray: NArray[Double] = {
    val vals: NArray[Double] = new NArray[Double](rows * columns)
    var i:Int = 0
    while (i < rows) {
      var j:Int = 0
      while (j < columns) {
        vals( i + j*rows) = values(i)(j)
        j = j + 1
      }
      i = i + 1
    }
    vals
  }

  /** Make a one-dimensional row packed copy of the internal array.
    *
    * @return Matrix elements packed in a one-dimensional array by rows.
    */
  def rowPackedArray: NArray[Double] = {
    val vals: NArray[Double] = new NArray[Double](rows * columns)
    var i: Int = 0
    while (i < rows) {
      var j: Int = 0
      while (j < columns) {
        vals( i*columns + j) = values(i)(j)
        j = j + 1
      }
      i = i + 1
    }
    vals
  }

  /** Get row dimension.
    *
    * @return m, the number of rows.
    */
  def rowDimension: Int = rows

  /** Get column dimension.
    *
    * @return n, the number of columns.
    */
  def columnDimension: Int = columns

  /** Get a single element.
    *
    * @param r Row index.
    * @param c Column index.
    * @return A(i,j)
    * @throws ArrayIndexOutOfBoundsException
    */
  inline def apply(r: Int, c: Int): Double = values(r)(c)

  /** Set a single element.
   *
   * @param r     Row index.
   * @param c     Column index.
   * @param value values(i,j).
   * @throws ArrayIndexOutOfBoundsException
   */
  inline def update(r: Int, c: Int, value: Double): Unit = values(r)(c) = value

  /** Get a submatrix.
   *
   * @tparam M1 the number of rows
   * @tparam N1 the number of columns
   * @param r0 Initial row index
   * @param c0 Initial column index
   * @return A(i0:i1,j0:j1)
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def subMatrix[M1 <: Int, N1 <: Int](r0: Int, c0: Int)(using ValueOf[M1], ValueOf[N1]): Matrix[M1, N1] = {
    val r1:Int = r0 + valueOf[M1]
    val c1:Int = c0 + valueOf[N1]
    new Matrix[M1, N1](
      NArray.tabulate[NArray[Double]](r1 - r0)(
        (r: Int) => NArray.tabulate[Double](c1 - c0)(
          (c: Int) => values(r + r)(c0 + c)
        )
      )
    )
  }

  /** Get a submatrix.
    *
    * @param rowIndices Array of row indices.
    * @param columnIndices Array of column indices.
    * @return A(r(:),c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def subMatrix[M1 <: Int, N1 <: Int](
    rowIndices: NArray[Int], columnIndices: NArray[Int]
  )(using ValueOf[M1], ValueOf[N1]): Matrix[M1, N1] = {
    dimensionCheck(valueOf[M1], rowIndices.length)
    dimensionCheck(valueOf[N1], columnIndices.length)
    new Matrix[M1, N1](
      NArray.tabulate[NArray[Double]](rowIndices.length)(
        (r: Int) => NArray.tabulate[Double](columnIndices.length)(
          (c: Int) => values(rowIndices(r))(columnIndices(c))
        )
      )
    )
  }

  /** Get a submatrix.
    *
    * @param r0 Initial row index
    * @param columnIndices  Array of column indices.
    * @return A(i0:i1,c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix[M1 <: Int, N1 <: Int](r0: Int, columnIndices: NArray[Int])(using ValueOf[M1], ValueOf[N1]): Matrix[M1, N1] = {
    val r1:Int = r0 + valueOf[M1]
    dimensionCheck(valueOf[N1], columnIndices.length)
    new Matrix(
      NArray.tabulate[NArray[Double]](r1 - r0 + 1)(
        (r: Int) => NArray.tabulate[Double](columnIndices.length)(
          (c: Int) => values(r + r0)(columnIndices(c))
        )
      )
    )
  }

  /** Get a submatrix.
    *
    * @param r  Array of row indices.
    * @param c0 Initial column index
    * @param c1 Final column index
    * @return A(r(:),j0:j1)
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def getMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], c0: Int)(using ValueOf[M1], ValueOf[N1]): Matrix[M1, N1] = {
    val c1 = c0 + valueOf[N1]
    new Matrix(
      NArray.tabulate[NArray[Double]](rowIndices.length)(
        (r: Int) => NArray.tabulate[Double](c1 - c0 + 1)(
          (c: Int) => values(rowIndices(r))(c + c0)
        )
      )
    )
  }

  /** Set a submatrix.
   * @param r0 Initial row index
   * @param c0 Initial column index
   * @param mtrx a metrix of lesser or equal dimension to this matrix
   * @param ValueOf[M1] Row dimension of mtrx
   * @param ValueOf[N1] Column dimension of mtrx
   * @tparam M1 Row dimension of mtrx
   * @tparam N1 Column dimension of mtrx
   */
  def setMatrix[M1 <: Int, N1 <: Int](r0: Int, c0: Int, mtrx: Matrix[M1, N1])(using ValueOf[M1], ValueOf[N1]): Unit = {
    val r1: Int = valueOf[M1]
    val c1: Int = valueOf[N1]
    var r:Int = r0
    while (r <= r1) {
      var c = c0
      while (c <= c1) {
        values(r)(c) = mtrx(r-r0,c-c0)
        c = c + 1
      }
      r = r + 1
    }
  }

  /** Set a submatrix.
    *
    * @param rowIndices Array of row indices.
    * @param columnIndices Array of column indices.
    * @param X A(r(:),c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], columnIndices: NArray[Int], X: Matrix[M1, N1]): Unit = {
    var i:Int = 0
    while (i < rowIndices.length) {
      var j:Int = 0
      while (j < columnIndices.length) {
        values(rowIndices(i))(columnIndices(j)) = X(i, j)
        j = j + 1
      }
      i = i + 1
    }
  }

  /** Set a submatrix.
    *
    * @param rowIndices  Array of row indices.
    * @param c0 Initial column index
    * @param c1 Final column index
    * @param X  A(r(:),j0:j1)
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], c0: Int, X: Matrix[M1, N1])(using ValueOf[M1], ValueOf[N1]): Unit = {
    val c1:Int = c0 + valueOf[N1]
    var r:Int = 0
    while (r < rowIndices.length) {
      var c:Int = c0
      while (c <= c1) {
        values(rowIndices(r))(c) = X(r, c - c0)
        c = c + 1
      }
      r = r + 1
    }
  }

  /** Set a submatrix.
    *
    * @param r0 Initial row index
    * @param r1 Final row index
    * @param columnIndices  Array of column indices.
    * @param X  A(i0:i1,c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix[M1 <: Int, N1 <: Int](r0: Int, columnIndices: NArray[Int], X: Matrix[M1, N1])(using ValueOf[M1], ValueOf[N1]): Unit = {
    val r1:Int = r0 + valueOf[M1]
    var r:Int = r0
    while (r <= r1) {
      var c:Int = 0
      while (c < columnIndices.length) {
        values(r)(columnIndices(c)) = X(r - r0, c)
        c = c + 1
      }
      r = r + 1
    }
  }

  /** Matrix transpose.
    *
    * @return Mᵀ
    */
  def transpose: Matrix[N, M] = new Matrix[N, M](
    NArray.tabulate[NArray[Double]](columns)(
      (col:Int) => NArray.tabulate[Double](rows)(
        (row:Int) => values(row)(col)
      )
    )
  )

  /** One norm
    *
    * @return maximum column sum.
    */
  def norm1: Double = {
    var maxColumnSum:Double = Double.MinValue
    var c:Int = 0
    while (c < columns) {
      var columnSum:Double = 0.0
      var r:Int = 0
      while (r < rows) {
        columnSum += Math.abs(values(r)(c))
        r = r + 1
      }
      maxColumnSum = Math.max(maxColumnSum, columnSum)
      c = c + 1
    }

    maxColumnSum
  }

  /** Infinity norm
    *
    * @return maximum row sum.
    */
  def normInfinity: Double = {
    var maxRowSum:Double = Double.MinValue

    var r:Int = 0
    while (r < rows) {
      var rowSum:Double = 0.0
      var c:Int = 0
      while (c < columns) {
        rowSum += Math.abs(values(r)(c))
        c = c + 1
      }
      maxRowSum = Math.max(maxRowSum, rowSum)
      r = r + 1
    }

    maxRowSum
  }

  /** Frobenius norm
    *
    * @return sqrt of sum of squares of all elements.
    */
  def normFrobenius: Double = {
    var f:Double = Double.MinValue

    var r:Int = 0
    while (r < rows) {
      var c:Int = 0
      while (c < columns) {
        f = hypot(f, values(r)(c))
        c = c + 1
      }
      r = r + 1
    }

    f
  }

  inline def + (B: Matrix[M, N]): Matrix[M, N] = copy.add(B)

  inline def += (B: Matrix[M, N]): Matrix[M, N] = add(B)

  /** A = A + B
    *
    * @param B another matrix
    * @return A + B
    */
  def add(B: Matrix[M, N]): Matrix[M, N] = {
    var r:Int = 0
    while (r < rows) {
      var c:Int = 0
      while (c < columns) {
        values(r)(c) = values(r)(c) + B(r, c)
        c = c + 1
      }
      r = r + 1
    }
    this
  }

  /** Unary minus
   *
   * @return -A
   */
  inline def unary_- : Matrix[M, N] = * ( -1.0 )

  inline def - (B: Matrix[M, N]): Matrix[M, N] = copy.subtract(B)

  inline def -= (B: Matrix[M, N]): Matrix[M, N] = subtract(B)

  /** A = A - B
    *
    * @param B another matrix
    * @return A - B
    */
  def subtract(B: Matrix[M, N]): Matrix[M, N] = {
    var r:Int = 0
    while (r < rows) {
      var c:Int = 0
      while (c < columns) {
        values(r)(c) = values(r)(c) - B(r, c)
        c = c + 1
      }
      r = r + 1
    }
    this
  }

  /** Multiply a matrix by a scalar, C = s*A
    *
    * @param s scalar
    * @return s*A
    */
  inline def * (s: Double): Matrix[M, N] = copy.times(s)

  inline def += (s:Double):Matrix[M, N] = times(s)

  /** Multiply a matrix by a scalar in place, A = s*A
    *
    * @param s scalar
    * @return replace A by s*A
    */
  def times(s: Double): Matrix[M, N] = {
    var r:Int = 0
    while (r < rows) {
      var c:Int = 0
      while (c < columns) {
        values(r)(c) = s * values(r)(c)
        c = c + 1
      }
      r = r + 1
    }
    this
  }

  def * [V <: Int](thatMatrix: Matrix[N, V])(using ValueOf[V]): Matrix[M, V] = {
    given v:Int = Math.min(valueOf[M], valueOf[V])
    times(thatMatrix)
  }

  /** Linear algebraic matrix multiplication, A * B
    *
    * @param b another matrix
    * @return Matrix product, A * B
    * @throws IllegalArgumentException Matrix inner dimensions must agree.
    */
  def times[V <: Int](b: Matrix[N, V])(using ValueOf[V]): Matrix[M, V] = {
    //if (b.rows != columns) throw new IllegalArgumentException("Matrix inner dimensions must agree.")

    val X:Matrix[M, V] = Matrix.zeros[M, V]

    val Bcolj = new Array[Double](columns)

    var j:Int = 0
    while (j < b.columns) {
      var k:Int = 0
      while (k < columns) {
        Bcolj(k) = b(k, j)
        k = k + 1
      }
      var i:Int = 0
      while (i < rows) {
        val Arowi = values(i)
        var s:Double = 0.0
        k = 0
        while (k < columns) {
          s += Arowi(k) * Bcolj(k)
          k = k + 1
        }
        X(i, j) = s
        i = i + 1
      }
      j = j + 1
    }
    X
  }

  /** Matrix trace.
    *
    * @return sum of the diagonal elements.
    */
  def trace: Double = {
    var t = 0.0
    var i:Int = 0
    while (i < Math.min(rows, columns)) {
      t += values(i)(i)
      i = i + 1
    }
    t
  }

  val mn:Int = rows * columns
  type MN = mn.type
  def asVector: Vec[MN] = {
    if (columns == 1 || rows == 1) {
      rowPackedArray.asInstanceOf[Vec[MN]]
    } else throw CannotExpressMatrixAsVector(this)
  }

  def dim: String = s"dim(${rows}x$columns)"

  override def toString: String = {
    val sb: StringBuilder = StringBuilder()
    var r: Int = 0;
    while (r < values.length) {
      sb.append("\n")
      var c: Int = 0;
      while (c < values(0).length) {
        sb.append(s"${values(r)(c)}, ")
        c = c + 1
      }
      r = r + 1
    }
    sb.toString()
  }

}
