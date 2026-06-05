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

import narr.*
import slash.*
import slash.vector.runtime.RTVec

/**
  * This library is fundamentally an adaptation of the Java Mat library, JaMa, by MathWorks Inc. and the National Institute of Standards and Technology.
  */

object RTMat {


  /**
   * Create a new RTMatrix from a MatrixData object.
   * @param md a matrix data object.
   * @return a new RTMatrix.
   */
  def apply(md:MatrixData):RTMat = new RTMat(md)

  /**
   * Create a new RTMatrix from an Array of RTVec values.
   * @param arr2d an Array of RTVec values.
   * @return a new RTMatrix.
   */
  def apply(arr2d:NArray[RTVec]):RTMat = {
    new RTMat(MatrixData(arr2d.asInstanceOf[NArray[NArray[Double]]]))
  }

  /** Construct a matrix from a copy of an array.
   *
   * @param values array of Doubles.
   * @return a copy of the array.
   * @throws IllegalArgumentException All rows must have the same length
   */
  def copyFrom(m:Int, n:Int, values: NArray[Double]): RTMat = {
    new RTMat(new LinearizedMatrixData(m, n, narr.copy[Double](values)))
  }

  /** Construct a matrix from a copy of an array.
   *
   * @param values 2D array of Doubles.
   * @return a copy of the 2D array.
   * @throws IllegalArgumentException All rows must have the same length
   */
  def copyFrom(values: NArray[NArray[Double]]): RTMat = new RTMat(MatrixData(util.copy(values)))

  /** Generates an MxN matrix which consists of elements randomized between [-1.0, 1.0] inclusive.
   *
   * @param m number of rows.
   * @param n number of columns.
   * @return an MxN matrix which consists of elements randomized between [-1.0, 1.0] inclusive.
   */
  inline def random(m:Int, n:Int): RTMat = random(m, n, slash.Random.defaultRandom)

  /**
   * Generates an MxN matrix which consists of elements randomized between [-1.0, 1.0] inclusive.
   * @param r optional random instance.
   * @return An MxN matrix with uniformly distributed random elements.
   */
  inline def random(m:Int, n:Int, r:scala.util.Random):RTMat = {
    random(m, n, slash.interval.`[]`(-1.0, 1.0), r)
  }

  /** Generate matrix with random elements
   *
   * @param m the number of rows.
   * @param n the number of columns.
   * @param minNorm Minimum random generated value allowed, inclusive.
   * @param normMAX Maximum random generated value allowed, inclusive.
   * @return An MxN matrix with uniformly distributed random elements.
   */
  inline def random(
    m:Int,
    n:Int,
    minNorm:Double,
    normMAX:Double
  ): RTMat = random(m, n, minNorm, normMAX, slash.Random.defaultRandom)


  /** Generate matrix with random elements
   * @param m the number of rows.
   * @param n the number of columns.
   * @param minNorm Minimum random generated value allowed, inclusive.
   * @param normMAX Maximum random generated value allowed, inclusive.
   * @param r optional random instance.
   * @return An MxN matrix with uniformly distributed random elements.
   */
  inline def random(
    m:Int,
    n:Int,
    minNorm:Double,
    normMAX:Double,
    r:scala.util.Random
  ): RTMat = random(m, n, slash.interval.`[]`(minNorm, normMAX), r)

  /** Generate matrix with random elements
   *
   * @param m the number of rows.
   * @param n the number of columns.
   * @param interval from which to draw matrix component values.
   * @param r optional random instance.
   * @return An MxN matrix with uniformly distributed random elements.
   */
  def random(
    m:Int,
    n:Int,
    interval:slash.interval.Interval[Double],
    r: scala.util.Random
  ): RTMat = {
    new RTMat(
      if (util.linearizeable(m, n)) new LinearizedMatrixData(m, n, NArray.tabulate[Double](m * n)(_ => interval.random(r)))
      else new MatrixDataGrid(
        NArray.tabulate[NArray[Double]](m)(
          _ => NArray.tabulate[Double](n)(_ => interval.random(r))
        )
      )
    )
  }

  /** Generate identity matrix
   *
   * @param m the number of rows
   * @param n the number of columns
   * @return An MxN matrix with ones on the diagonal and zeros elsewhere.
   */
  inline def identity(m:Int, n:Int): RTMat = diagonal(m, n, 1.0)

  /**
   * Generate identity matrix scaled by value parameter.
   *
   * @param m number of rows.
   * @param n  number of columns.
   * @param value scalar multiplier
   * @return An MxN matrix with 'value' on the diagonal and zeros elsewhere.
   */

  def diagonal(m:Int, n:Int, value:Double): RTMat = new RTMat(util.diagonal(m, n, value))

  /**
   * Generate a square matrix with the supplied vector along the diagonal.
   * @param v a vector
   * @return
   */
  def diagonal(v:RTVec): RTMat = new RTMat(util.diagonal(v.asNativeArray))

  /**
   * Create a rectangular matrix with the given vector along the diagonal.
   * @param m the number of rows.
   * @param n the number of rows.
   * @param v a vector to place along the diagonal.
   * @return an MxN rectangular matrix with the given vector along the diagonal.
   */
  def diagonal(m:Int, n:Int, v: RTVec): RTMat = new RTMat(util.diagonal(m, n, v.asNativeArray) )

  /** Construct an MxN constant matrix.
   * @param m the number of rows.
   * @param n the number of rows.
   * @param value Fill the matrix with this scalar value.
   * @return an MxN constant matrix.
   */
  def fill(m:Int, n:Int, value: Double):RTMat = new RTMat(util.fill(m, n, value))

  /** Construct an MxN matrix of zeros.
   *
   * @param m the number of rows
   * @param n the number of columns
   */
  def zeros(m:Int, n:Int):RTMat = new RTMat(util.zeros(m, n))

  /** Construct an MxN matrix of ones.
   *
   * @param m the number of rows
   * @param n the number of columns
   */
  def ones(m:Int, n:Int): RTMat = new RTMat(util.fill(m, n, 1.0))

  /** Construct a matrix from a one-dimensional packed array
   *
   * @param values One-dimensional array of doubles, packed by rows.
   * @param m the number of rows
   * @param n the number of columns
   * @throws IllegalArgumentException NArray length must equal M * N
   */
  def apply(m:Int, n:Int, values: NArray[Double]):RTMat = new RTMat(new LinearizedMatrixData(m, n, values))

  /**
   *
   * @param values the matrix elements.
   * @param m the number of rows
   * @param n the number of columns
   * @return an M x N matrix consisting of values.
   */
  def apply(m:Int, n:Int, values: Double *):RTMat = {
    new RTMat(new LinearizedMatrixData(m, n, NArray[Double](values *)))
  }

}

class RTMat private (val values: MatrixData) {

  val rows: Int = values.rowDimension
  val columns: Int = values.columnDimension

  /** Get row dimension.
   *
   * @return m, the number of rows.
   */
  inline def rowDimension: Int = rows

  /** Get column dimension.
   *
   * @return n, the number of columns.
   */
  inline def columnDimension: Int = columns

  val MxN: Int = rows * columns

  /** Make a deep copy of a matrix
    */
  def copy: RTMat = new RTMat(values.copy)

//  /** Copy the internal two-dimensional array.
//    *
//    * @return Two-dimensional array copy of matrix elements.
//    */
//  inline def copyValues: NArray[Double] = narr.copy[Double](values)

  /** Make a one-dimensional column packed copy of the internal array.
    *
    * @return Mat elements packed in a one-dimensional array by columns.
    */
  def columnPackedNArray: NArray[Double] = values.columnPackedNArray

  /** Make a one-dimensional row packed copy of the internal array.
    *
    * @return Mat elements packed in a one-dimensional array by rows.
    */
  inline def rowPackedArray: NArray[Double] = values.rowPackedNArray

  /**
   * @param row the row of the matrix to return as a vector.
   * @return a copy of the specified matrix row in Vec[N] format.
   */
  inline def rowVector(row:Int):RTVec = RTVec.apply(values.getRow(row))

  /**
   * @return a copy of this matrix in the form of an array of row vectors.
   */
  inline def rowVectors: NArray[RTVec] = util.as2DNArray(values).asInstanceOf[NArray[RTVec]]

  /**
   * @param column the column of the matrix to return as a vector.
   * @return a copy of the specified matrix column in Vec[M] format.
   */
  inline def columnVector(column:Int):RTVec =  util.extractColumn(column, values).asInstanceOf[RTVec]

  /**
   * @return a copy of this matrix in the form of an array of column vectors.
   */
  inline def columnVectors: NArray[RTVec] = transpose.rowVectors

  /** Get a single element.
    *
    * @param r Row index.
    * @param c Column index.
    * @return A(r,c).
    * @throws ArrayIndexOutOfBoundsException if r, c lies outside the matrix dimensions.
    */
  inline def apply(r: Int, c: Int): Double = values(r, c)

  /** Set a single element.
   *
   * @param r     Row index.
   * @param c     Column index.
   * @return A(r,c).
   * @throws ArrayIndexOutOfBoundsException if r, c lies outside the matrix dimensions.
   */
  inline def update(r: Int, c: Int, value: Double): Unit = values(r, c) = value

  /** Get a submatrix.
   *
   * @param r0 Initial row index.
   * @param c0 Initial column index.
   * @param newRows Initial row index.
   * @param newColumns Initial column index.
   * @return A(r0:newRows,c0:newColumns).
   * @throws ArrayIndexOutOfBoundsException if submatrix indices lie outside the matrix dimensions.
   */
  def subMatrix(r0: Int, c0: Int, newRows:Int, newColumns:Int): RTMat = new RTMat(
    util.subMatrix(values, r0, c0, newRows, newColumns)
  )

  /** Get a submatrix.
    *
    * @param rowIndices Array of row indices.
    * @param columnIndices Array of column indices.
    * @return A(r(:),c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def subMatrix(rowIndices: NArray[Int], columnIndices: NArray[Int]): RTMat = new RTMat(
    util.subMatrix(values, rowIndices, columnIndices)
  )

  /** Get a submatrix.
    *
    * @param r0 Initial row index
    * @param columnIndices  Array of column indices.
    * @return A(i0:i1,c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def subMatrix(r0: Int, newRows:Int, columnIndices: NArray[Int]): RTMat = new RTMat(
    util.subMatrix(values, r0: Int, newRows, columnIndices: NArray[Int])
  )

  /** Get a submatrix.
    *
    * @param rowIndices  Array of row indices.
    * @param c0 Initial column index.
    * @param cEnd Final column index.
    * @return A(rowIndices(:),c0:c1).
    * @throws ArrayIndexOutOfBoundsException if submatrix indices lie outside the matrix dimensions.
    */
  def subMatrix(rowIndices: NArray[Int], c0: Int, cEnd:Int): RTMat = new RTMat(
    util.subMatrix(values, rowIndices, c0, cEnd)
  )

  /** Set a submatrix.
   * @param r0 Initial row index.
   * @param c0 Initial column index.
   * @param thatMatrix a matrix of lesser or equal dimension to this matrix.
   * @param newRows Row dimension of thatMatrix.
   * @param newColumns Column dimension of thatMatrix.
   */
  def setMatrix(r0: Int, c0: Int, thatMatrix: RTMat, newRows:Int, newColumns:Int): Unit = {
    util.setMatrix(values, r0, c0, thatMatrix.values, newRows, newColumns)
  }

  /** Set Matrix
   *
   * @param rowIndices Array of row indices.
   * @param columnIndices Array of column indices.
   * @param thatMatrix the donor matrix; A(rowIndices(:),columnIndices(:)).
   * @throws ArrayIndexOutOfBoundsException if submatrix indices lie outside the matrix dimensions.
   */
  def setMatrix(rowIndices: NArray[Int], columnIndices: NArray[Int], thatMatrix: RTMat): Unit = {
    util.setMatrix(values, rowIndices, columnIndices, thatMatrix.values)
  }

  /**
   * Set a submatrix.
   *
   * @param rowIndices Array of row indices
   * @param c0 Initial column index
   * @param thatMatrix the donor matrix
   */
  def setMatrix(rowIndices: NArray[Int], c0: Int, thatMatrix: RTMat): Unit = {
    util.setMatrix(values, rowIndices, c0, thatMatrix.values)
  }

  /**
   * Set a submatrix.
   *
   * @param r0 Initial row index
   * @param columnIndices Array of column indices.
   * @param thatMatrix donor matrix
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def setMatrix(r0: Int, columnIndices: NArray[Int], thatMatrix: RTMat): Unit = {
    util.setMatrix(values, r0, columnIndices, thatMatrix.values)
  }

  /**
   * Mat transpose.
   *
   * @return Mᵀ
   */
  def transpose: RTMat = new RTMat(values.transpose)

  /**
   * One norm
   *
   * @return maximum column sum.
   */
  def norm1: Double = util.norm1(values)

  /** Infinity norm
   *
   * @return maximum row sum.
   */
  def normInfinity: Double = util.normInfinity(values)

  /** Frobenius norm
    *
    * @return sqrt of sum of squares of all elements.
    */
  def normFrobenius: Double = util.normFrobenius(values)

  def + (B: RTMat): RTMat = {
    val out = copy
    out.add(B)
    out
  }

  inline def += (B: RTMat): Unit = add(B)

  /** A = A + B
    *
    * @param B another matrix
    * @return A + B
    */
  def add(B: RTMat): Unit = util.add(values, B.values)

  /** Unary minus
   *
   * @return -A
   */
  inline def unary_- : RTMat = * ( -1.0 )

  def - (B: RTMat): RTMat = {
    val out = copy
    out.subtract(B)
    out
  }

  inline def -= (B: RTMat): Unit = subtract(B)

  /** A = A - B
   *
   * @param B another matrix
   * @return A - B
   */
  def subtract(B: RTMat): Unit = util.subtract(values, B.values)

  /** A = A + d
   * add a scalar to every element of a matrix.
   * @param d a scalar
   * @return A + d
   */
  inline def addScalar(d: Double): Unit = util.addScalar(values, d)

  /** Multiply a matrix by a scalar, C = A * s
    *
    * @param s scalar
    * @return A * s
    */
  def * (s: Double): RTMat = {
    val out = copy
    out.times(s)
    out
  }

  inline def *= (s:Double): Unit = times(s)

  /** Add a scalar to a matrix, C = A + s
    *
    * @param s scalar
    * @return A + s
    */
  def + (s: Double): RTMat = {
    val out = copy
    out.addScalar(s)
    out
  }

  inline def += (s:Double): Unit = addScalar(s)

  def - (s: Double): RTMat = {
    val out = copy
    out.addScalar(-s)
    out
  }

  inline def -= (s: Double): Unit = addScalar(-s)

  /** Multiply a matrix by a scalar in place, A = s*A
    *
    * @param s scalar
    * @return replace A by s*A
    */
  inline def times(s: Double): Unit = util.times(values, s)

  //inline def * (v: slash.vectorf.VecF[N]): slash.vectorf.VecF[N] = times(v)

//  inline def times (v: slash.vectorf.VecF[N]): slash.vectorf.VecF[N] = {
//    util.times(values, v.asNativeArray).asInstanceOf[slash.vectorf.VecF[N]]
//  }

  inline def * (v: RTVec): RTVec = times(v)

  inline def times (v: RTVec): RTVec = util.times(values, v.asNativeArray).asInstanceOf[RTVec]

  inline def * (thatMatrix: RTMat): RTMat = {
    dimensionCheck(columns, thatMatrix.rowDimension)
    times(thatMatrix)
  }

  /** Linear algebraic matrix multiplication, A * B
    *
    * @param b another matrix
    * @return Mat product, A * B
    * @throws IllegalArgumentException Mat inner dimensions must agree.
    */
  def times(b: RTMat): RTMat = new RTMat(util.times(values, b.values))

  /** Pointwise multiplication (hadamard product) A * B
    *
    * @param B another matrix
    * @return A * B (where * is the Hadamard Product)
    */
  inline def pointwiseMultiply(B: RTMat): Unit = util.pointwiseMultiply(values, B.values)

  /** Pointwise multiplication (hadamard product) A * B
    *
    * @param B another matrix
    * @return A * B (where * is the Hadamard Product)
    */
  def pointwiseMultiplied(B: RTMat): RTMat = {
    val out = copy
    out.pointwiseMultiply(B)
    out
  }

  /** Mat trace.
    *
    * @return sum of the diagonal elements.
    */
  inline def trace: Double = util.trace(values)

  def kronecker(b: RTMat): RTMat = new RTMat(util.kronecker(values, b.values))

  inline def dim: String = s"dim(${rows}x$columns)"

  def concatenateRows(m: RTMat): RTMat = new RTMat(MatrixData.concatenateRows(values, m.values))

//  inline def concatenateRows(m: Mat[? <: Int, ? <: Int]): NArray[Double] = MatrixData.concatenateRows(values, m.values)

  def concatenateColumns(m: RTMat): RTMat = new RTMat(MatrixData.concatenateColumns(values, m.values))

//  inline def concatenateColumns(m: Mat[? <: Int, ? <: Int]): NArray[Double] = {
//    util.concatenateColumns(rows, columns, values, m.columns, m.values)
//  }

  def asNativeArray2D: NArray[NArray[Double]] = rowVectors.asInstanceOf[NArray[NArray[Double]]]

  def strictEquals(obj: Any): Boolean = {
    obj match {
      case that: Mat[?, ?] =>
        if (this.columns == that.columns && this.rows == that.rows) {
          util.strictEquals(values, that.values)
        } else false
      case _ => false
    }
  }

  def upperTriangular: RTMat = new RTMat(util.upperTriangular(values))

  def lowerTriangular: RTMat = new RTMat(util.lowerTriangular(values))

  def diagonalVector: RTVec = util.diagonalVector(values).asInstanceOf[RTVec]

  def asVector: RTVec = values.asInstanceOf[RTVec]

  inline def copyAsVector: RTVec = rowPackedArray.asInstanceOf[RTVec]

//  /** cast matrix as RTMat with dimensions r x c
//   *
//   * @param r new vertical dimension
//   * @param c new horizontal dimension
//   * @return same values, but recast to rXc
//   */
//  def reshape(r: Int, c: Int): RTMat = {
//    dimensionCheck(r + c, rows + columns)
//    new RTMat(values)
//  }

  /** values as a Vector.
   */
  def flatten: slash.vector.runtime.RTVec = slash.vector.runtime.RTVec(values.rowPackedNArray)

  /**
   * Extension Methods for Square Matrices.
   */

  private lazy val svd: decomposition.RTSV = decomposition.RTSV(this)
  private lazy val lu: decomposition.RTLU = decomposition.RTLU(this)
  private lazy val qr: decomposition.RTQR = decomposition.RTQR(this)

  /**
   * https://en.wikipedia.org/wiki/Invertible_matrix
   *
   * Computes the inverse of Square Mat m.
   *
   * @throws RuntimeException( "Mat is singular." )
   * @return the inverse of matrix m
   */
  def inverse: RTMat = {
    dimensionCheck(rows, columns)
    solve(RTMat.identity(rows, columns))
  }

  /** Solve a * x = b
   *
   * @param b right hand side
   * @return x = Mat[MN, V] such that a * x = b
   */
  def solve(b: RTMat): RTMat = {
    if (rows == columns) lu.solve(b)
    else qr.solve(b)
  }

  /** Mat determinant
   * https://en.wikipedia.org/wiki/Determinant
   * the determinant is nonzero if and only if the matrix is invertible and the linear map represented by the matrix is an isomorphism
   *
   * @return the determinant of this matrix.
   */
  def determinant: Double = lu.determinant

  /** Solve b * m = I[N, N]
   * m = Mat[M, N] with M > N and Rank = N, has a left inverse b = Mat[N, M] such that b * m = I[N, N]
   *
   * @return b = Mat[N, M] the Left Inverse of Mat m.
   */
  def leftInverse: RTMat = {
    if (rows < columns) {
      throw new RuntimeException("Matrix row dimension must equal or exceed column dimension to compute leftInverse.")
    } else if (rank == columns) {
      svd.V * svd.S_inverse * svd.U.transpose
    } else throw new RuntimeException("Matrix must have full rank to compute leftInverse.")
  }

  /** Two norm
   *
   * @return maximum singular value.
   */
  def norm2: Double = svd.norm2

  /** Mat rank
   *
   * @return effective numerical rank, obtained from SV.
   */
  def rank: Int = svd.rank

  /** Mat condition (2 norm)
   *
   * @return ratio of largest to smallest singular value.
   */
  def cond: Double = svd.cond

  /**
   * m = Mat[M, N] with M < N and Rank = M, has a right inverse b = Mat[N, M] such that m * b = Identity[M, M]
   *
   * @return the Right Inverse of this RTMat.
   */
  lazy val rightInverse: RTMat = {
    if (rows > columns) throw new RuntimeException("Matrix column dimension must exceed row dimension to compute rightInverse.")
    else if (rank == rows) qr.solve(RTMat.identity(columns, columns))
    else throw new RuntimeException("Matrix must have full row rank to compute rightInverse.")
  }

  def render(
    format: MatFormat = MatFormat.DEFAULT,
    alignment: Function2[MatrixData, MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): StringBuilder = {
    format.render(values, alignment, sb)
  }

  override def toString: String = csv

  def csv: String = csv(MatFormat.UNALIGNED, new StringBuilder())

  def csv(
    alignment: Function2[MatrixData, MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): String = render(MatFormat.CSV, alignment, sb).toString

  def tsv: String = tsv(MatFormat.UNALIGNED, new StringBuilder())

  def tsv(
    alignment: Function2[MatrixData, MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): String = render(MatFormat.TSV, alignment, sb).toString

}