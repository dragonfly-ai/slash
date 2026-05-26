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

package slash.matrix

import scala.math.hypot
import narr.*
import slash.squareInPlace

/**
 * All methods in slash.matrix.util assume valid dimensions.
 * These methods extremely unsafe.  Consider using Mat[M,N] or RTMat instead of using these directly.
 */
package object util {

  /**
   * Compute the linear index of a 2D coordinate of a matrix array literal.
   * @param r the matrix row.
   * @param c the matrix column.
   * @param columns number of columns in the matrix.
   * @return
   */
  def lindex(r: Int, c: Int, columns: Int): Int = (r * columns) + c

  /**
   * Generate matrix of zeros
   * @param m number of rows
   * @param n number of columns
   * @return a NArray representing the values of an MxN matrix of zeros.
   */
  inline def zeros(m: Int, n: Int): NArray[Double] = new DoubleArray(m * n)

  /** Construct an MxN constant matrix.
   *
   * @param value Fill the matrix with this scalar value.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return a NArray representing the values of an MxN constant matrix.
   */
  inline def fill(m:Int, n:Int, value: Double): NArray[Double] = NArray.fill[Double](m * n)(value)

  /**
   * Generate identity matrix scaled by value parameter.
   *
   * @param m number of rows
   * @param n number of columns
   * @param value scalar multiplier
   * @return An MxN matrix with ones on the diagonal and zeros elsewhere.
   */

  def diagonal(m:Int, n:Int, value: Double): NArray[Double] = {
    val out: NArray[Double] = new DoubleArray(m*n)
    val min: Int = Math.min(m, n)
    var i: Int = 0
    while (i < min) {
      out(lindex(i, i, m)) = value
      i = i + 1
    }
    out
  }

  /**
   * Generate a square matrix with the supplied vector along the diagonal.
   *
   * @param v a vector
   * @return NArray[Double] represenging an v.dimension X v.dimension square matrix with the supplied vector along the diagonal.
   */
  def diagonal(v: NArray[Double]): NArray[Double] = {
    val out: NArray[Double] = new NArray[Double](squareInPlace(v.length))
    var i: Int = 0
    while (i < v.length) {
      out(lindex(i, i, v.length)) = v(i)
      i = i + 1
    }
    out
  }

  /**
   * Create an array of values representing a rectangular matrix with the given vector along the diagonal.
   *
   * @param v a vector to place along the diagonal.  If v.dimension < Min(M, N) then zeros will make up the remaining elements on the diagonal.
   *          If v.dimension > Min(M, N) then the resulting matrix will exclude its extra elements.
   * @tparam M the number of rows.
   * @tparam N the number of rows.
   * @tparam D the dimension of the vector.
   * @return an NArray[Double] representing an MxN rectangular matrix with the given vector along the diagonal.
   */
  def diagonal(rows:Int, columns:Int, v: NArray[Double]): NArray[Double] = {

    val out: NArray[Double] = new NArray[Double](rows * columns)

    var i: Int = 0
    while (i < Math.min(v.length, Math.min(rows, columns))) {
      out(lindex(i, i, i)) = v(i)
      i = i + 1
    }
    out
  }


  /** Flatten a 2-D array.
   *
   * @param arr2d Two-dimensional array of doubles.  arr2d(row)(column).
   * @throws IllegalArgumentException All rows must have the same length
   */

  def flatten(arr2d:NArray[NArray[Double]]):NArray[Double] = {
    val rows:Int = arr2d.length
    val columns:Int = arr2d(0).length
    val out:NArray[Double] = new NArray[Double](rows * columns)
    var i:Int = 0
    var r:Int = 0
    while (r < rows) {
      var c:Int = 0
      while (c < columns) {
        out(i) = arr2d(r)(c)
        i += 1
        c += 1
      }
      r += 1
    }
    out
  }

  /**
   * Make a one-dimensional column packed copy of the internal array.
   * @param rows
   * @param columns
   * @param values a row packed copy of the values of a rowsXcolumns matrix.
   * @return Mat elements packed in a one-dimensional array by columns.
   */
  def columnPackedNArray(rows:Int, columns:Int, values: NArray[Double]):NArray[Double] = {
    val vs: NArray[Double] = new NArray[Double](values.length)
    var i: Int = 0
    while (i < rows) {
      var j: Int = 0
      while (j < columns) {
        vs(i + j * rows) = values(lindex(i, j, columns))
        j = j + 1
      }
      i = i + 1
    }
    vs
  }

  /**
   * extract a copy of a row.
   * @param row the row of the matrix to return as a vector.
   * @return an copy of the specified matrix row in NArray[Double] format.
   */
  inline def extractRow(columns: Int, row: Int, values:NArray[Double]): NArray[Double] = {
    values.asInstanceOf[NArr[Double]].slice(row * columns, (row * columns) + columns).asInstanceOf[NArray[Double]]
  }

  /**
   * extract a column from a row packed array of matrix values.
   * @param columns the number of columns.
   * @param column the desired column.
   * @param values the row packed matrix array.
   * @return a copy of the specified matrix column in NArray[Double] format.
   */
  def extractColumn(rows:Int, columns: Int, column: Int, values:NArray[Double]): NArray[Double] = {
    NArray.tabulate[Double](rows)((r: Int) => values(lindex(r, column, columns)))
  }

  /**
   * convert a row packed matrix into a 2D Array.
   * @param rows number of rows.
   * @param columns number of columns.
   * @param values a row packed copy of the matrix values.
   * @return a 2D array representing the matrix values.
   */
  def as2DNArray(rows:Int, columns: Int, values:NArray[Double]):NArray[NArray[Double]] = NArray.tabulate[NArray[Double]](rows)(
    (row: Int) => extractRow(columns, row, values)
  )

  /** Get a submatrix.
   *
   * @tparam M1 the number of rows
   * @tparam N1 the number of columns
   * @param r0 Initial row index
   * @param c0 Initial column index
   * @return A(i0:i1,j0:j1)
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */

  /**
   *
   * @param columns from the original matrix
   * @param values original matrix values
   * @param subRows number of rows in the submatrix
   * @param subColumns number of columns in the submatrix
   * @param r0 Initial row index
   * @param c0 Initial column index
   * @return rowpacked array of submatrix values A(r0:r0 + subRows,c0:c0 + subColumns)
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def subMatrix(columns: Int, values:NArray[Double], r0: Int, c0: Int, subRows: Int, subColumns: Int): NArray[Double] = {

    val rEnd: Int = r0 + subRows
    val cEnd: Int = c0 + subColumns

    val out: NArray[Double] = new NArray[Double](subRows * subColumns)
    var i: Int = 0
    var r: Int = r0
    while (r < rEnd) {
      var c: Int = c0
      while (c < cEnd) {
        out(i) = values(lindex(r, c, columns))
        i += 1
        c += 1
      }
      r += 1
    }
    out
  }

  /** Get a submatrix from arrays of row indices and column indices.
   *
   * @param rowIndices    Array of row indices.
   * @param columnIndices Array of column indices.
   * @return A(r(:),c(:))
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def subMatrix(columns:Int, values: NArray[Double], rowIndices: NArray[Int], columnIndices: NArray[Int]):NArray[Double] = {
    val out: NArray[Double] = new NArray[Double](rowIndices.length * columnIndices.length)
    var i: Int = 0
    var ri: Int = 0;
    while (ri < rowIndices.length) {
      var ci: Int = 0;
      while (ci < columnIndices.length) {
        out(i) = values(lindex(rowIndices(ri), columnIndices(ci), columns))
        i += 1
        ci += 1
      }
      ri += 1
    }
    out
  }

  /** Get a submatrix.
   *
   * @param r0 Initial row index
   * @param columnIndices Array of column indices.
   * @return A(i0:i1,c(:))
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def subMatrix(columns:Int, values:NArray[Double], r0: Int, subRows: Int, columnIndices: NArray[Int]): NArray[Double] = {
    val rEnd: Int = r0 + subRows

    val out: NArray[Double] = new NArray[Double](subRows * columnIndices.length)
    var i: Int = 0
    var ri: Int = r0;
    while (ri < rEnd) {
      var ci: Int = 0;
      while (ci < columnIndices.length) {
        out(i) = values(lindex(ri, columnIndices(ci), columns))
        i += 1
        ci += 1
      }
      ri += 1
    }
    out
  }


  /** Get a submatrix.
   *
   * @param columns Number of columns in original matrix.
   * @param rowIndices  Array of row indices.
   * @param c0 Initial column index.
   * @param subColumns number of columns to extract
   * @return A(r(:),j0:j1).
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def subMatrix(columns:Int, values:NArray[Double], subColumns:Int, rowIndices: NArray[Int], c0: Int): NArray[Double] = {
    val cEnd = c0 + subColumns
    val vs: NArray[Double] = new NArray[Double](rowIndices.length * subColumns)
    var i: Int = 0
    var ri: Int = 0; while (ri < rowIndices.length) {
      var ci: Int = c0; while (ci < cEnd) {
        vs(i) = values( lindex( rowIndices(ri), ci, columns ) )
        i += 1
        ci += 1
      }
      ri += 1
    }
    vs
  }

  /**
   * Set a submatrix.
   *
   * @param columns number of columns in original matrix.
   * @param values values of the original matrix.
   * @param r0 starting row in the original matrix.
   * @param c0 starting column in the original matrix.
   * @param donorValues new values
   * @param newRows number of rows in the new values.
   * @param newColumns number of columns in the new values.
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def setMatrix(
    columns:Int, values:NArray[Double], r0: Int, c0: Int, // original
    donorValues:NArray[Double], newRows:Int, newColumns:Int // new
  ): Unit = {
    val rEnd: Int = newRows + r0
    val cEnd: Int = newColumns + c0
    var r: Int = r0
    while (r < rEnd) {
      var c = c0
      while (c < cEnd) {
        values(lindex(r, c, columns)) = donorValues(lindex(r - r0, c - c0, newColumns))
        c = c + 1
      }
      r = r + 1
    }
  }

  /**
   * Set a submatrix.
   *
   * @param columns number of columns in the original matrix.
   * @param original original matrix values.
   * @param rowIndices Array of row indices.
   * @param columnIndices Array of column indices.
   * @param donorValues new values
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def setMatrix(
    columns:Int, values:NArray[Double], rowIndices: NArray[Int], columnIndices: NArray[Int],
    donorValues: NArray[Double]
  ): Unit = {
    var i:Int = 0
    while (i < rowIndices.length) {
      var j:Int = 0
      while (j < columnIndices.length) {
        values(lindex(rowIndices(i), columnIndices(j), columns)) = donorValues(lindex(i, j, columnIndices.length))
        j = j + 1
      }
      i = i + 1
    }
  }

  /**
   * Set a submatrix.
   *
   * @param columns number of columns in the original matrix.
   * @param values original matrix values.
   * @param rowIndices Array of row indices.
   * @param c0 Initial column index.
   * @param donorValues new values.
   * @param newColumns number of columns in the donor matrix.
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def setMatrix(
    columns:Int, values:NArray[Double], rowIndices: NArray[Int],
    c0: Int, donorValues: NArray[Double], newColumns:Int
  ): Unit = {
    val columnEnd: Int = c0 + newColumns
    var r: Int = 0
    while (r < rowIndices.length) {
      var c: Int = c0
      while (c < columnEnd) {
        values(lindex(rowIndices(r), c, columns)) = donorValues(lindex(r, c - c0, newColumns))
        c = c + 1
      }
      r = r + 1
    }
  }

  /**
   * Set a submatrix.
   *
   * @param columns number of columns in receiving matrix.
   * @param values values of the receiving matrix.
   * @param r0 initial row index.
   * @param columnIndices Array of column indices.
   * @param donorValues donor matrix values.
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def setMatrix(columns:Int, values:NArray[Double], r0:Int, columnIndices:NArray[Int], donorValues:NArray[Double]): Unit = {
    val r1: Int = r0 + columnIndices.length
    var r: Int = r0
    while (r < r1) {
      var c: Int = 0
      while (c < columnIndices.length) {
        values(lindex(r, columnIndices(c), columns)) = donorValues(lindex(r - r0, c, columnIndices.length))
        c = c + 1
      }
      r = r + 1
    }
  }

  /**
   * One norm
   *
   * @param rows number of rows.
   * @param columns number of columns.
   * @param values matrix values.
   * @return maximum column sum
   */
  def norm1(rows:Int, columns:Int, values:NArray[Double]): Double = {
    var maxColumnSum: Double = Double.MinValue
    var c: Int = 0
    while (c < columns) {
      var columnSum: Double = 0.0
      var r: Int = 0
      while (r < rows) {
        columnSum += Math.abs(values(lindex(r, c, columns)))
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
  def normInfinity(rows:Int, columns:Int, values:NArray[Double]): Double = {
    var maxRowSum: Double = Double.MinValue
    var r: Int = 0
    while (r < rows) {
      var rowSum: Double = 0.0
      var c: Int = 0
      while (c < columns) {
        rowSum += Math.abs(values(lindex(r, c, columns)))
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
  def normFrobenius(values:NArray[Double]): Double = {
    var f: Double = 0.0
    var i: Int = 0
    while (i < values.length) {
      f = hypot(f, values(i))
      i = i + 1
    }
    f
  }

  /** A = A + B
   * Add matrix B to Matrix A in place.
   * @param a matrix values
   * @param b matrix values to add
   */
  def add(a:NArray[Double], b:NArray[Double]): Unit = {
    var i: Int = 0
    while (i < a.length) {
      a(i) = a(i) + b(i)
      i = i + 1
    }
  }

  /** A = A - B
   * Subtract matrix B from Matrix A in place.
   * @param a matrix values
   * @param b matrix values to subtract
   */
  def subtract(a:NArray[Double], b:NArray[Double]): Unit = {
    var i: Int = 0
    while (i < a.length) {
      a(i) = a(i) - b(i)
      i = i + 1
    }
  }

  /** A = A + d
   * add a scalar to every element of a matrix.
   * @param values matrix values
   * @param d a scalar
   * @return A + d
   */
  def addScalar(values:NArray[Double], d:Double): Unit = {
    var i: Int = 0
    while (i < values.length) {
      values(i) += d
      i += 1
    }
  }

  /**
   * Multiply a matrix by a scalar in place, A = s*A
   *
   * @param values the values of a matrix.
   * @param s scalar.
   * @return replace Matrix A by s*A.
   */
  def times(values:NArray[Double], s: Double): Unit = {
    var i: Int = 0
    while (i < values.length) {
      values(i) = values(i) * s
      i += 1
    }
  }

  /**
   * Multiply a vector of floats by a matrix.
   *
   * @param mat Matrix values.
   * @param vec vector values with float valued components.
   * @return mat * vec
   */
  def times(mat:NArray[Double], vec:NArray[Float]): NArray[Float] = {
    val a: NArray[Float] = new NArray[Float](vec.length)
    var i: Int = 0
    var ai: Int = 0
    while (i < mat.length) {
      var dotProduct = 0.0
      var offset: Int = 0
      while (offset < vec.length) {
        dotProduct = dotProduct + (mat(i + offset) * vec(offset))
        offset = offset + 1
      }
      a(ai) = dotProduct.toFloat
      ai = ai + 1
      i = i + vec.length
    }
    a
  }

  /**
   * Multiply a vector of floats by a matrix.
   *
   * @param mat matrix values.
   * @param vec vector values.
   * @return mat * vec
   */
  def times (mat:NArray[Double], vec: NArray[Double]): NArray[Double] = {
    val a:NArray[Double] = new NArray[Double](vec.length)
    var i:Int = 0
    var ai:Int = 0
    while (i < mat.length) {
      var dotProduct = 0.0
      var offset: Int = 0
      while (offset < vec.length) {
        dotProduct = dotProduct + (mat(i + offset) * vec(offset))
        offset = offset + 1
      }
      a(ai) = dotProduct
      ai = ai + 1
      i = i + vec.length
    }
    a
  }


  /** Linear algebraic matrix multiplication, A * B
   *
   * @param b another matrix
   * @return Mat product, A * B
   * @throws IllegalArgumentException Mat inner dimensions must agree.
   */

  /**
   *
   * @param aRows number of rows in matrix A.
   * @param aColumns number of columns in matrix A.
   * @param a matrix A.
   * @param bColumns number of columns in matrix B.
   * @param b matrix B.
   * @return matrix product A X B
   * @throws IllegalArgumentException Mat inner dimensions must agree.
   */
  def times(aRows:Int, aColumns:Int, a:NArray[Double], bColumns:Int, b:NArray[Double]): NArray[Double] = {

    val X:NArray[Double] = new NArray[Double](aRows * bColumns)

    val Bcolj = new NArray[Double](aColumns)

    var j:Int = 0
    while (j < bColumns) {
      var k:Int = 0
      while (k < aColumns) {
        Bcolj(k) = b(lindex(k, j, bColumns))
        k = k + 1
      }
      var i:Int = 0
      while (i < aRows) {
        var s:Double = 0.0
        k = 0
        while (k < aColumns) {
          s += a(lindex(i, k, aColumns)) * Bcolj(k)
          k = k + 1
        }
        X(lindex(i, j, bColumns)) = s
        i = i + 1
      }
      j = j + 1
    }
    X
  }

  /**
   * Pointwise multiplication (Hadamard product) A * B
   * @param a Matrix A
   * @param b Matrix B
   * @throws IllegalArgumentException Mat dimensions must agree.
   */
  def pointwiseMultiply(a:NArray[Double], b:NArray[Double]): Unit = {
    var i: Int = 0
    while (i < a.length) {
      a(i) = a(i) * b(i)
      i = i + 1
    }
  }

  /**
   * Kronecker product.
   *
   * @param aRows number of rows in matrix A.
   * @param aColumns number of columns in matrix A.
   * @param a Matrix A
   * @param bRows number of rows in matrix B.
   * @param bColumns number of columns in matrix B.
   * @param b Matrix B
   * @return the Kronecker product of two matrices.
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def kronecker(aRows:Int, aColumns:Int, a:NArray[Double], bRows:Int, bColumns:Int, b:NArray[Double]): NArray[Double] = {
    // (using ValueOf[V * M], ValueOf[W * N])
    //b[V <: Int, W <: Int]
    //val X: Mat[V * M, W * N] = Mat.zeros[V * M, W * N]
    val X:NArray[Double] = new NArray[Double](aRows * bRows * aColumns * bColumns)
    val xColumns:Int = aColumns * bColumns

    var i1: Int = 0; while (i1 < aRows) {
      val iBase = i1 * aRows
      var j1: Int = 0; while (j1 < aColumns) {
        val jBase = j1 * aColumns
        val k = a(lindex(i1, j1,aColumns))
        var i2: Int = 0; while (i2 < bRows) {
          var j2: Int = 0; while (j2 < bColumns) {
            X(lindex(iBase + i2, jBase + j2, xColumns)) = k * b(lindex(i2, j2, bColumns))
            j2 = j2 + 1
          }
          i2 = i2 + 1
        }
        j1 = j1 + 1
      }
      i1 = i1 + 1
    }
    X
  }

  /**
   * Mat trace.
   *
   * @return sum of the diagonal elements.
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def trace(rows:Int, columns:Int, values:NArray[Double]): Double = {
    val end:Int = Math.min(rows, columns)
    var t = 0.0
    var i: Int = 0
    while (i < end) {
      t += values(lindex(i, i, columns))
      i = i + 1
    }
    t
  }

  /**
   * Concatenate the rows of two matrices: A and B.
   * @param a .values of Matrix A
   * @param b values of Matrix B.
   * @return a matrix that combines the rows of A with the rows of B.
   */
  def concatenateRows(a:NArray[Double], b:NArray[Double]): NArray[Double] = {
    val out: NArray[Double] = new NArray[Double](a.length + b.length)
    narr.native.NArray.copyDoubleArray(a, 0, out, 0, a.length)
    narr.native.NArray.copyDoubleArray(b, 0, out, a.length, b.length)
    out
  }

  /**
   * Concatenate the columns of two matrices: A and B.
   *
   * @param aRows number of rows in Matrix A.
   * @param aColumns number of columns in Matrix A.
   * @param a values of Matrix A.
   * @param bColumns number of columns in Matrix B.
   * @param b values of Matrix A.
   * @return a matrix that combines the columns of A with the columns of B.
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def concatenateColumns(aRows:Int, aColumns:Int, a:NArray[Double], bColumns:Int, b:NArray[Double]): NArray[Double] = {
    val out: NArray[Double] = new NArray[Double](a.length + b.length)
    var r: Int = 0
    var i: Int = 0
    while (r < aRows) {
      narr.native.NArray.copyDoubleArray(a, r * aColumns, out, i, aColumns)
      i = i + aColumns
      narr.native.NArray.copyDoubleArray(b, r * bColumns, out, i, bColumns)
      i = i + bColumns
      r = r + 1
    }
    out
  }

  /**
   * Create an upper triangular matrix from this matrix.
   *
   * @param rows number of matrix rows.
   * @param columns number of matrix columns.
   * @param values the values of the matrix.
   * @return an upper triangular matrix
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def upperTriangular(rows:Int, columns:Int, values:NArray[Double]): NArray[Double] = {
    val out: NArray[Double] = narr.copy[Double](values)
    var i = 0
    while (i < rows) {
      var j = 0
      while (j < i) {
        out(lindex(i, j, columns)) = 0.0
        j += 1
      }
      i += 1
    }
    out
  }

  /**
   * Create a lower triangular matrix from this matrix.
   *
   * @param rows number of matrix rows.
   * @param columns number of matrix columns.
   * @param values the values of the matrix.
   * @return a lower triangular matrix
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def lowerTriangular(rows:Int, columns:Int, values:NArray[Double]): NArray[Double] = {
    val out: NArray[Double] = narr.copy[Double](values)
    var i = 0
    while(i < rows){
      var j = i+1
      while(j < columns){
        out(lindex(i,j,columns)) = 0.0
        j += 1
      }
      i += 1
    }
    out
  }

  /**
   * A vector constructed from the diagonal of a matrix.
   *
   * @param rows number of matrix rows.
   * @param columns number of matrix columns.
   * @param values the values of the matrix.
   * @return a vector constructed from the diagonal of a matrix.
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def diagonalVector(rows:Int, columns:Int, values:NArray[Double]): NArray[Double] = {
    val dim: Int = math.min(rows, columns)
    val arr: NArray[Double] = new NArray[Double](dim)
    var i = 0
    while(i < dim) {
      arr(i) = values(lindex(i,i,columns))
      i += 1
    }
    arr
  }

  /**
   * Compare all elements of two NArray[Double]s to test for equality.
   *
   * Does not consider matrix dimensions.
   *
   * @param a matrix or vector values.
   * @param b matrix or vector values.
   * @return true if the two arrays have identically valued elements.
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def strictEquals(a:NArray[Double], b:NArray[Double]): Boolean = {
    var i:Int = 0
    var same:Boolean = true
    while (i < a.length && same) {
      same = same && (a(i) == b(i))
      i = i + 1
    }
    same
  }
}
