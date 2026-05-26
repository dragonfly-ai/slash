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
  /** Construct a matrix from a copy of an array.
   *
   * @param values array of doubles.
   * @throws IllegalArgumentException All rows must have the same length
   */
  inline def copyFrom(m:Int, n:Int, values: NArray[Double]): RTMat = new RTMat(m, n, narr.copy[Double](values))

  /**
   * Generates an MxN matrix which consists of elements randomized between [-1.0, 1.0] inclusive.
   *
   * @tparam N the number of columns
   * @return
   */
  inline def random(m:Int, n:Int): RTMat = random(m, n, slash.Random.defaultRandom)

  /**
   * Generates an MxN matrix which consists of elements randomized between [-1.0, 1.0] inclusive.
   * @param r optional random instance.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with uniformly distributed random elements.
   */
  inline def random(m:Int, n:Int, r:scala.util.Random):RTMat = {
    random(m, n, slash.interval.`[]`(-1.0, 1.0), r)
  }

  /** Generate matrix with random elements
   *
   * @param minNorm Minimum random generated value allowed, inclusive.
   * @param normMAX Maximum random generated value allowed, inclusive.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with uniformly distributed random elements.
   */
  inline def random(
    m:Int,
    n:Int,
    minNorm:Double,
    normMAX:Double
  ): RTMat = random(m, n, minNorm, normMAX, slash.Random.defaultRandom)


  /** Generate matrix with random elements
   * @param minNorm Minimum random generated value allowed, inclusive.
   * @param normMAX Maximum random generated value allowed, inclusive.
   * @param r optional random instance.
   * @tparam M the number of rows
   * @tparam N the number of columns
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
   * @param interval from which to draw matrix component values.
   * @param r       optional random instance.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with uniformly distributed random elements.
   */
  inline def random(
    m:Int,
    n:Int,
    interval:slash.interval.Interval[Double],
    r: scala.util.Random
  ): RTMat = new RTMat(m, n, NArray.tabulate[Double]( m * m )( _ => interval.random(r) ))

  /** Generate identity matrix
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with ones on the diagonal and zeros elsewhere.
   */
  inline def identity(m:Int, n:Int): RTMat = diagonal(m, n, 1.0)

  /**
   * Generate identity matrix scaled by value parameter.
   *
   * @param value scalar multiplier
   * @param x$2 implicit
   * @param x$3 implicit
   * @tparam M number of rows.
   * @tparam N  number of columns.
   * @return An MxN matrix with 'value' on the diagonal and zeros elsewhere.
   */

  inline def diagonal(m:Int, n:Int, value:Double): RTMat = new RTMat(m, n, util.diagonal(m, m, value))

  /**
   * Generate a square matrix with the supplied vector along the diagonal.
   * @param v a vector
   * @return
   */
  inline def diagonal(v:RTVec): RTMat = new RTMat(v.dimension, v.dimension, util.diagonal(v.asNativeArray))

  /**
   * Create a rectangular matrix with the given vector along the diagonal.
   * @param v a vector to place along the diagonal.
   * @tparam M the number of rows.
   * @tparam N the number of rows.
   * @tparam D the dimension of the vector.  Must equal either M or N.
   * @return an MxN rectangular matrix with the given vector along the diagonal.
   */
  inline def diagonal(m:Int, n:Int, v: RTVec): RTMat = new RTMat(m, n, util.diagonal(m, n, v.asNativeArray) )

  /** Construct a matrix from a 2-D array.
   *
   * @param arr2d Two-dimensional array of doubles.  arr2d(row)(column).
   */

  inline def apply(m:Int, n:Int, arr2d:NArray[RTVec]):RTMat = {
    new RTMat(m, n, util.flatten(arr2d.asInstanceOf[NArray[NArray[Double]]]))
  }

  /** Construct an MxN constant matrix.
   * @param value Fill the matrix with this scalar value.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return an MxN constant matrix.
   */
  inline def fill(m:Int, n:Int)(value: Double):RTMat = new RTMat(m, n, util.fill(m, n, value))

  /** Construct an MxN matrix of zeros.
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   */
  inline def zeros(m:Int, n:Int):RTMat = new RTMat(m, n, util.zeros(m, n))

  /** Construct an MxN matrix of ones.
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   */
  inline def ones(m:Int, n:Int): RTMat = new RTMat(m, n, util.fill(m, n, 1.0))

  /** Construct a matrix from a one-dimensional packed array
   *
   * @param values One-dimensional array of doubles, packed by rows.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @throws IllegalArgumentException NArray length must equal M * N
   */
  def apply(m:Int, n:Int, values: NArray[Double]):RTMat = new RTMat(m, n, values)

  /**
   *
   * @param values the matrix elements.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return an M x N matrix consisting of values.
   */
  def apply(m:Int, n:Int, values: Double *):RTMat = {
    dimensionCheck(values.size, m * n)
    new RTMat(m, n, NArray[Double](values *))
  }

  // support left multiply by scalar
  extension (d: Double) {
    def * (rtm: RTMat): RTMat = rtm * d // same as right multiply
  }

}

class RTMat(rows:Int, columns:Int, val values: NArray[Double]) {

  val MxN: Int = rows * columns

  require(rows * columns == values.length, s"Product of $rows x $columns != ${values.length}")

  //inline def lindex(r:Int, c:Int):Int = (r * columns) + c

  /** Make a deep copy of a matrix
    */
  inline def copy: RTMat = new RTMat(rows, columns, copyValues)

  /** Copy the internal two-dimensional array.
    *
    * @return Two-dimensional array copy of matrix elements.
    */
  inline def copyValues: NArray[Double] = narr.copy[Double](values)

  /** Make a one-dimensional column packed copy of the internal array.
    *
    * @return Mat elements packed in a one-dimensional array by columns.
    */
  def columnPackedNArray: NArray[Double] = util.columnPackedNArray(rows, columns, values)

  /** Make a one-dimensional row packed copy of the internal array.
    *
    * @return Mat elements packed in a one-dimensional array by rows.
    */
  inline def rowPackedArray: NArray[Double] = copyValues

  /**
   * @param row the row of the matrix to return as a vector.
   * @return a copy of the specified matrix row in Vec[N] format.
   */
  inline def rowVector(row:Int):RTVec = RTVec.apply(util.extractRow(columns, row, values))

  /**
   * @return a copy of this matrix in the form of an array of row vectors.
   */
  inline def rowVectors: NArray[RTVec] = util.as2DNArray(rows, columns, values).asInstanceOf[NArray[RTVec]]

  /**
   * @param column the column of the matrix to return as a vector.
   * @return a copy of the specified matrix column in Vec[M] format.
   */
  inline def columnVector(column:Int):RTVec =  util.extractColumn(rows, columns, column, values).asInstanceOf[RTVec]

  /**
   * @return a copy of this matrix in the form of an array of column vectors.
   */
  inline def columnVectors: NArray[RTVec] = transpose.rowVectors

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

  /** Get a single element.
    *
    * @param r Row index.
    * @param c Column index.
    * @return A(i,j)
    * @throws ArrayIndexOutOfBoundsException
    */
  inline def apply(r: Int, c: Int): Double = values(util.lindex(r, c, columns))

  /** Set a single element.
   *
   * @param r     Row index.
   * @param c     Column index.
   * @param value values(i,j).
   * @throws ArrayIndexOutOfBoundsException
   */
  inline def update(r: Int, c: Int, value: Double): Unit = values(util.lindex(r, c, columns)) = value

  /** Get a submatrix.
   *
   * @tparam M1 the number of rows
   * @tparam N1 the number of columns
   * @param r0 Initial row index
   * @param c0 Initial column index
   * @return A(i0:i1,j0:j1)
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  inline def subMatrix[M1 <: Int, N1 <: Int](r0: Int, c0: Int)(using ValueOf[M1], ValueOf[N1]): Mat[M1, N1] = new Mat[M1, N1](
    util.subMatrix(columns, values, r0, c0, valueOf[M1], valueOf[N1])
  )

  /** Get a submatrix.
    *
    * @param rowIndices Array of row indices.
    * @param columnIndices Array of column indices.
    * @return A(r(:),c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  inline def subMatrix[M1 <: Int, N1 <: Int](
    rowIndices: NArray[Int], columnIndices: NArray[Int]
  )(using ValueOf[M1], ValueOf[N1]): Mat[M1, N1] = new Mat[M1, N1](
    util.subMatrix(columns, values, rowIndices, columnIndices)
  )

  /** Get a submatrix.
    *
    * @param r0 Initial row index
    * @param columnIndices  Array of column indices.
    * @return A(i0:i1,c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def subMatrix[M1 <: Int, N1 <: Int](r0: Int, columnIndices: NArray[Int])(using ValueOf[M1], ValueOf[N1]): Mat[M1, N1] = {
    require(
      valueOf[N1] == columnIndices.length,
      s"column dimension of ${valueOf[N1]} != column indices array length of ${columnIndices.length}"
    )
    new Mat[M1, N1](
      util.subMatrix(columns, values, r0: Int, valueOf[M1], columnIndices: NArray[Int])
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
  def subMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], c0: Int)(using ValueOf[M1], ValueOf[N1]): Mat[M1, N1] = {
    new Mat[M1, N1](util.subMatrix(columns, values, valueOf[N1], rowIndices, c0))
  }

  /** Set a submatrix.
   * @param r0 Initial row index
   * @param c0 Initial column index
   * @param thatMatrix a metrix of lesser or equal dimension to this matrix
   * @param ValueOf[M1] Row dimension of thatMatrix
   * @param ValueOf[N1] Column dimension of thatMatrix
   * @tparam M1 Row dimension of thatMatrix
   * @tparam N1 Column dimension of thatMatrix
   */
  def setMatrix[M1 <: Int, N1 <: Int](r0: Int, c0: Int, thatMatrix: Mat[M1, N1])(using ValueOf[M1], ValueOf[N1]): Unit = {
    util.setMatrix(
      columns, values, r0, c0, // original
      thatMatrix.values, valueOf[M1], valueOf[N1] // new
    )
  }

  /**
   *
   * @param rowIndices Array of row indices.
   * @param columnIndices Array of column indices.
   * @param thatMatrix the donor matrix; A(r(:),c(:)).
   * @tparam M1 number of rows in the other matrix.
   * @tparam N1 number of columns in the other matrix.
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def setMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], columnIndices: NArray[Int], thatMatrix: Mat[M1, N1]): Unit = {
    util.setMatrix(columns, values, rowIndices, columnIndices, thatMatrix.values)
  }

  /**
   * Set a submatrix.
   *
   * @param rowIndices Array of row indices
   * @param c0 Initial column index
   * @param thatMatrix the donor matrix
   * @tparam M1 number of rows of the new Matrix.
   * @tparam N1 number of columns of the new Matrix.
   */
  def setMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], c0: Int, thatMatrix: Mat[M1, N1]): Unit = {
    util.setMatrix(
      columns, values, rowIndices,
      c0, thatMatrix.values, thatMatrix.columns
    )
  }

  /**
   * Set a submatrix.
   *
   * @param r0 Initial row index
   * @param columnIndices Array of column indices.
   * @param thatMatrix donor matrix
   * @tparam M1 number of rows of the new Matrix.
   * @tparam N1 number of columns of the new Matrix.
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def setMatrix[M1 <: Int, N1 <: Int](r0: Int, columnIndices: NArray[Int], thatMatrix: Mat[M1, N1]): Unit = {
    util.setMatrix(columns, values, r0, columnIndices, thatMatrix.values)
  }

  /**
   * Mat transpose.
   *
   * @return Mᵀ
   */
  def transpose: RTMat = new RTMat(columns, rows, columnPackedNArray)

  /**
   * One norm
   *
   * @return maximum column sum.
   */
  def norm1: Double = util.norm1(rows, columns, values)

  /** Infinity norm
   *
   * @return maximum row sum.
   */
  def normInfinity: Double = util.normInfinity(rows, columns, values)

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
    dimensionCheck(rows, thatMatrix.rowDimension)
    dimensionCheck(columns, thatMatrix.columnDimension)
    times(thatMatrix)
  }

  /** Linear algebraic matrix multiplication, A * B
    *
    * @param b another matrix
    * @return Mat product, A * B
    * @throws IllegalArgumentException Mat inner dimensions must agree.
    */
  inline def times(b: RTMat): RTMat = new RTMat(
    rows, b.columnDimension, util.times(rows, columns, values, b.columnDimension, b.values)
  )

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

  inline def kronecker(b: RTMat): RTMat = {
    new RTMat(
      b.rowDimension * rows,
      b.columnDimension * columns,
      util.kronecker(rows, columns, values, b.rowDimension, b.columnDimension, b.values)
    )
  }

  /** Mat trace.
    *
    * @return sum of the diagonal elements.
    */
  inline def trace: Double = util.trace(rows, columns, values)

  inline def dim: String = s"dim(${rows}x$columns)"

  inline def concatenateRows(m: RTMat): RTMat = new RTMat(
    rows, columns + m.columnDimension, util.concatenateRows(values, m.values)
  )

  inline def concatenateRows(m: Mat[? <: Int, ? <: Int]): NArray[Double] = util.concatenateRows(values, m.values)

  def concatenateColumns(m: RTMat): RTMat = new RTMat(
    rows, columns + m.columnDimension, util.concatenateColumns(rows, columns, values, m.columnDimension, m.values)
  )

  inline def concatenateColumns(m: Mat[? <: Int, ? <: Int]): NArray[Double] = {
    util.concatenateColumns(rows, columns, values, m.columns, m.values)
  }

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

  inline def upperTriangular: RTMat = new RTMat(rows, columns, util.upperTriangular(rows, columns, values))

  inline def lowerTriangular: RTMat = new RTMat(rows, columns, util.lowerTriangular(rows, columns, values))

  inline def diagonalVector: RTVec = util.diagonalVector(rows, columns, values).asInstanceOf[RTVec]

  def asVector: RTVec = values.asInstanceOf[RTVec]

  inline def copyAsVector: RTVec = NArray.copy[Double](values).asInstanceOf[RTVec]

  /** cast matrix as RTMat with dimensions r x c
   *
   * @param r new vertical dimension
   * @param c new horizontal dimension
   * @return same values, but recast to rXc
   */
  def reshape(r: Int, c: Int): RTMat = {
    dimensionCheck(r + c, rows + columns)
    new RTMat(r, c, values)
  }

  /** values as a Vector.
   */
  def flatten: slash.vector.runtime.RTVec = slash.vector.runtime.RTVec(values)

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
    alignment: Function4[Int, Int, NArray[Double], MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): StringBuilder = {
    format.render(rows, columns, values, alignment, sb)
  }

  override def toString: String = csv

  def csv: String = csv(MatFormat.UNALIGNED, new StringBuilder()).toString

  def csv(
    alignment: Function4[Int, Int, NArray[Double], MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): String = render(MatFormat.CSV, alignment, sb).toString

  def tsv: String = tsv(MatFormat.UNALIGNED, new StringBuilder()).toString

  def tsv(
    alignment: Function4[Int, Int, NArray[Double], MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): String = render(MatFormat.TSV, alignment, sb).toString

}