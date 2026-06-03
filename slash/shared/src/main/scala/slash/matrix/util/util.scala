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
import narr.NArrayBuilder.MAX_NArraySize
import slash.exceptions.CannotLinearizeMatrixData

/**
 * All methods in slash.matrix.util assume valid dimensions.
 * These methods extremely unsafe.  Consider using Mat[M,N] or RTMat instead of using these directly.
 */
package object util {

//  /**
//   * Compute the linear index of a 2D coordinate of a matrix array literal.
//   * @param r the matrix row.
//   * @param c the matrix column.
//   * @param columns number of columns in the matrix.
//   * @return
//   */
//  def lindex(r: Int, c: Int, columns: Int): Int = (r * columns) + c


  /**
   * Can the underlying MatrixData fit into a 1D NArray[Double]?
   *
   * @return true if the elements of this array can fit into a single 1D NArray[Double], otherwise false.
   */
  def linearizeable(rows:Int, cols:Int):Boolean = MAX_NArraySize - rows >= cols

  /**
   * Generate matrix of zeros
   * @param m number of rows
   * @param n number of columns
   * @return a NArray representing the values of an MxN matrix of zeros.
   */
  inline def zeros(m: Int, n: Int): MatrixData = MatrixData(m, n)

  /** Construct an MxN constant matrix.
   *
   * @param value Fill the matrix with this scalar value.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return a NArray representing the values of an MxN constant matrix.
   */
  inline def fill(m:Int, n:Int, value: Double): MatrixData = {
    if (linearizeable(m, n)) new LinearizedMatrixData(m, n, NArray.fill[Double](m * n)(value))
    else {
      val row = NArray.fill[Double](n)(value)
      val out = new NArray[NArray[Double]](m)
      out(0) = row
      var r:Int = 1
      while (r < out.length) {
        out(r) = NArray.copy[Double](row)
        r = r + 1
      }
      new MatrixDataGrid(out)
    }
  }

  /**
   * Generate identity matrix scaled by value parameter.
   *
   * @param m number of rows
   * @param n number of columns
   * @param value scalar multiplier
   * @return An MxN matrix with ones on the diagonal and zeros elsewhere.
   */

  def diagonal(m:Int, n:Int, value: Double): MatrixData = {
    val out: MatrixData = zeros(m, n)
    val min: Int = Math.min(m, n)
    var i: Int = 0
    while (i < min) {
      out(i, i) = value
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
  def diagonal(v: NArray[Double]): MatrixData = {
    val out: MatrixData = zeros(v.length, v.length)
    var i: Int = 0
    while (i < v.length) {
      out(i, i) = v(i)
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
  def diagonal(rows:Int, columns:Int, v: NArray[Double]): MatrixData = {

    val out: MatrixData = zeros(rows, columns)

    val end = Math.min(v.length, Math.min(rows, columns))

    var i: Int = 0
    while (i < end) {
      out(i, i) = v(i)
      i = i + 1
    }
    out
  }


  /** Flatten a 2-D array.
   *
   * @param arr2d Two-dimensional array of doubles.  arr2d(row)(column).
   * @throws IllegalArgumentException All rows must have the same length.
   * @throws CannotLinearizeMatrixData Number of elements cannot exceed maximum array size.
   */

  def flatten(arr2d:NArray[NArray[Double]]):NArray[Double] = {
    val rows:Int = arr2d.length
    val columns:Int = arr2d(0).length
    if (linearizeable(rows, columns)) {
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
    } else throw CannotLinearizeMatrixData(rows, columns)
  }

  /**
   * extract a column from a row packed array of matrix values.
   * @param columns the number of columns.
   * @param column the desired column.
   * @param values the row packed matrix array.
   * @return a copy of the specified matrix column in NArray[Double] format.
   */
  def extractColumn(column:Int, values:MatrixData): NArray[Double] = {
    NArray.tabulate[Double](values.rowDimension)((r: Int) => values(r, column))
  }

  /**
   * convert a row packed matrix into a 2D Array.
   * @param rows number of rows.
   * @param columns number of columns.
   * @param values a row packed copy of the matrix values.
   * @return a 2D array representing the matrix values.
   */
  def as2DNArray(values:MatrixData):NArray[NArray[Double]] = {
    NArray.tabulate[NArray[Double]](values.rowDimension)(
      (row: Int) => values.getRow(row)
    )
  }

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
  def subMatrix(values:MatrixData, r0: Int, c0: Int, subRows: Int, subColumns: Int): MatrixData = {

    val rEnd: Int = r0 + subRows
    val cEnd: Int = c0 + subColumns

    val out: MatrixData = MatrixData(subRows, subColumns)
    var r: Int = r0
    var sR: Int = 0
    while (r < rEnd) {
      var c: Int = c0
      var sC:Int = 0
      while (c < cEnd) {
        out(sR, sC) = values(r, c)
        c += 1
        sC += 1
      }
      r += 1
      sR += 1
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
  def subMatrix(values: MatrixData, rowIndices: NArray[Int], columnIndices: NArray[Int]):MatrixData = {
    val out: MatrixData = MatrixData(rowIndices.length, columnIndices.length)
    var ri: Int = 0
    var sRi: Int = 0
    while (ri < rowIndices.length) {
      var ci: Int = 0
      var sCi:Int = 0
      while (ci < columnIndices.length) {
        out(sRi, sCi) = values(rowIndices(ri), columnIndices(ci))
        ci += 1
        sCi += 1
      }
      ri += 1
      sRi += 1
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
  def subMatrix(values:MatrixData, r0: Int, subRows: Int, columnIndices: NArray[Int]): MatrixData = {
    val rEnd: Int = r0 + subRows

    val out: MatrixData = MatrixData(subRows, columnIndices.length)
    var i: Int = 0
    var ri: Int = r0
    var sRi: Int = 0
    while (ri < rEnd) {
      var ci: Int = 0
      var sCi: Int = 0
      while (ci < columnIndices.length) {
        out(sRi, sCi) = values(ri, columnIndices(ci))
        ci += 1
        sCi += 1
      }
      ri += 1
      sRi += 1
    }
    out
  }


  /** Get a submatrix.
   *
   * @param rowIndices  Array of row indices.
   * @param subColumns number of columns to extract
   * @param c0 Initial column index.
   * @return A(r(:),j0:j1).
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def subMatrix(values:MatrixData, rowIndices: NArray[Int], subColumns:Int, c0: Int): MatrixData = {
    val cEnd = c0 + subColumns
    val vs: MatrixData = MatrixData(rowIndices.length, subColumns)
    var ri: Int = 0
    var sRi: Int = 0
    while (ri < rowIndices.length) {
      var ci: Int = c0
      var sCi: Int = 0
      while (ci < cEnd) {
        vs(sRi, sCi) = values(rowIndices(ri), ci)
        ci += 1
        sCi += 1
      }
      ri += 1
      sRi += 1
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
  def setMatrix(values:MatrixData, r0: Int, c0: Int, donorValues:MatrixData, newRows:Int, newColumns:Int): Unit = {
    val rEnd: Int = newRows + r0
    val cEnd: Int = newColumns + c0
    var r: Int = r0
    while (r < rEnd) {
      var c = c0
      while (c < cEnd) {
        values(r, c) = donorValues(r - r0, c - c0)
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
  def setMatrix(values:MatrixData, rowIndices: NArray[Int], columnIndices: NArray[Int], donorValues: MatrixData): Unit = {
    var i:Int = 0
    while (i < rowIndices.length) {
      var j:Int = 0
      while (j < columnIndices.length) {
        values(rowIndices(i), columnIndices(j)) = donorValues(i, j)
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
  def setMatrix(values:MatrixData, rowIndices: NArray[Int], c0: Int, donorValues: MatrixData): Unit = {
    val columnEnd: Int = c0 + donorValues.columnDimension
    var r: Int = 0
    while (r < rowIndices.length) {
      var c: Int = c0
      while (c < columnEnd) {
        values(rowIndices(r), c) = donorValues(r, c - c0)
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
  def setMatrix(values:MatrixData, r0:Int, columnIndices:NArray[Int], donorValues:MatrixData): Unit = {
    val r1: Int = r0 + columnIndices.length
    var r: Int = r0
    while (r < r1) {
      var c: Int = 0
      while (c < columnIndices.length) {
        values(r, columnIndices(c)) = donorValues(r - r0, c)
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
  def norm1(values:MatrixData): Double = {
    var maxColumnSum: Double = Double.MinValue
    var c: Int = 0
    while (c < values.columnDimension) {
      var columnSum: Double = 0.0
      var r: Int = 0
      while (r < values.rowDimension) {
        columnSum += Math.abs(values(r, c))
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
  def normInfinity(values:MatrixData): Double = {
    var maxRowSum: Double = Double.MinValue
    var r: Int = 0
    while (r < values.rowDimension) {
      var rowSum: Double = 0.0
      var c: Int = 0
      while (c < values.columnDimension) {
        rowSum += Math.abs(values(r, c))
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
  def normFrobenius(values:MatrixData): Double = {
    var f: Double = 0.0
    var r: Int = 0
    while (r < values.rowDimension) {
      var c: Int = 0
      while (c < values.columnDimension) {
        f = hypot(f, values(r, c))
        c += 1
      }
      r += 1
    }
    f
  }

  /** A = A + B
   * Add matrix B to Matrix A in place.
   *
   * Use at your own risk!  Assumes equal dimensions for A and B.  Performs no validation.
   *
   * @param a matrix values
   * @param b matrix values to add
   */
  def add(a:MatrixData, b:MatrixData): Unit = {
    var r: Int = 0
    while (r < a.rowDimension) {
      var c: Int = 0
      while (c < a.columnDimension) {
        a(r, c) = a(r, c) + b(r, c)
        c += 1
      }
      r += 1
    }
  }

  /** A = A - B
   * Subtract matrix B from Matrix A in place.
   *
   * Use at your own risk!  Assumes equal dimensions for A and B.  Performs no validation.
   *
   * @param a matrix values
   * @param b matrix values to subtract
   */
  def subtract(a:MatrixData, b:MatrixData): Unit = {
    var r: Int = 0
    while (r < a.rowDimension) {
      var c: Int = 0
      while (c < a.columnDimension) {
        a(r, c) = a(r, c) - b(r, c)
        c += 1
      }
      r += 1
    }
  }

  /** A = A + d
   * add a scalar to every element of a matrix.
   * @param values matrix values
   * @param d a scalar
   * @return A + d
   */
  def addScalar(values:MatrixData, d:Double): Unit = {
    var r: Int = 0
    while (r < values.rowDimension) {
      var c: Int = 0
      while (c < values.columnDimension) {
        values(r, c) += d
        c += 1
      }
      r += 1
    }
  }

  /**
   * Multiply a matrix by a scalar in place, A = s*A
   *
   * @param values the values of a matrix.
   * @param s scalar.
   * @return replace Matrix A by s*A.
   */
  def times(values:MatrixData, s: Double): Unit = {
    var r: Int = 0
    while (r < values.rowDimension) {
      var c: Int = 0
      while (c < values.columnDimension) {
        values(r, c) = values(r, c) * s
        c += 1
      }
      r += 1
    }
  }

  /**
   * Multiply a vector of floats by a matrix.
   *
   * @param mat Matrix values.
   * @param vec vector values with float valued components.
   * @return mat * vec
   */
  def times(mat:MatrixData, vec:NArray[Float]): NArray[Float] = {
    val out: NArray[Float] = new NArray[Float](vec.length)
    var r:Int = 0
    while (r < mat.rowDimension) {
      var c: Int = 0
      var product = 0.0
      while (c < vec.length) {
        product = product + mat(r, c) * vec(r)
        c += 1
      }
      out(r) = product.toFloat
      r += 1
    }
    out
  }

  /**
   * Multiply a vector by a matrix.
   *
   * @param mat matrix values.
   * @param vec vector values.
   * @return mat * vec
   */
  def times (mat:MatrixData, vec:NArray[Double]): NArray[Double] = {
    val out:NArray[Double] = new NArray[Double](vec.length)
    var r: Int = 0
    while (r < mat.rowDimension) {
      var c: Int = 0
      var product = 0.0
      while (c < vec.length) {
        product = product + mat(r, c) * vec(r)
        c += 1
      }
      out(r) = product.toFloat
      r += 1
    }
    out
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
  def times(a:MatrixData, b:MatrixData): MatrixData = {

    val X:MatrixData = MatrixData(a.rowDimension, b.columnDimension)

    val Bcolj = new NArray[Double](a.columnDimension)

    var j:Int = 0
    while (j < b.columnDimension) {
      var k:Int = 0
      while (k < a.columnDimension) {
        Bcolj(k) = b(k, j)
        k = k + 1
      }
      var i:Int = 0
      while (i < a.rowDimension) {
        var s:Double = 0.0
        k = 0
        while (k < a.columnDimension) {
          s += a(i, k) * Bcolj(k)
          k = k + 1
        }
        X(i, j) = s
        i = i + 1
      }
      j = j + 1
    }
    X
  }

  /**
   * Pointwise multiplication (Hadamard product) A * B
   * Assumes that both matrices have identical dimensions without any validation.
   *
   * @param a Matrix A
   * @param b Matrix B
   * @throws IllegalArgumentException Mat dimensions must agree.
   */
  def pointwiseMultiply(a:MatrixData, b:MatrixData): Unit = {
    var r: Int = 0
    while (r < a.rowDimension) {
      var c: Int = 0
      while (c < a.columnDimension) {
        a(r, c) = a(r, c) * b(r, c)
        c += 1
      }
      r += 1
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
  def kronecker(a:MatrixData, b:MatrixData): MatrixData = {
    val X:MatrixData = MatrixData(a.rowDimension * b.rowDimension, a.columnDimension * b.columnDimension)

    var i1: Int = 0
    while (i1 < a.rowDimension) {
      val iBase = i1 * a.rowDimension
      var j1: Int = 0
      while (j1 < a.columnDimension) {
        val jBase = j1 * a.columnDimension
        val k = a(i1, j1)
        var i2: Int = 0
        while (i2 < b.rowDimension) {
          var j2: Int = 0
          while (j2 < b.columnDimension) {
            X(iBase + i2, jBase + j2) = k * b(i2, j2)
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
  def trace(values:MatrixData): Double = {
    val end:Int = Math.min(values.rowDimension, values.columnDimension)
    var t = 0.0
    var i: Int = 0
    while (i < end) {
      t += values(i, i)
      i += 1
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

  def copy(values:NArray[NArray[Double]]): NArray[NArray[Double]] = {
    NArray.tabulate[NArray[Double]](values.length)(
      (r: Int) => narr.copy[Double](values(r))
    )
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
  def upperTriangular(values:MatrixData): MatrixData = {
    val out: MatrixData = values.copy
    var i = 0
    while (i < values.rowDimension) {
      var j = 0
      while (j < i) {
        out(i, j) = 0.0
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
  def lowerTriangular(values:MatrixData): MatrixData = {
    val out: MatrixData = values.copy
    var i = 0
    while(i < values.rowDimension){
      var j = i+1
      while(j < values.columnDimension){
        out(i,j) = 0.0
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
  def diagonalVector(values:MatrixData): NArray[Double] = {
    val dim: Int = math.min(values.rowDimension, values.columnDimension)
    val out: NArray[Double] = new NArray[Double](dim)
    var i = 0
    while(i < dim) {
      out(i) = values(i,i)
      i += 1
    }
    out
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
  def strictEquals(a:MatrixData, b:MatrixData): Boolean = {
    var r: Int = 0
    while (r < a.rowDimension) {
      var c: Int = 0
      while (c < a.columnDimension) {
        if (a(r, c) != b(r, c)) return false
        c += 1
      }
      r += 1
    }
    true
  }
}
