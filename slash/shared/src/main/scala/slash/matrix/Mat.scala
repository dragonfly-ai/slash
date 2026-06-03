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

import slash.*
import narr.*
import slash.vector.Vec

import scala.compiletime.ops.int.*

/**
  * This library is fundamentally an adaptation of the Java Mat library, JaMa, by MathWorks Inc. and the National Institute of Standards and Technology.
  */

object Mat {
  /** Construct a matrix from a copy of an array.
   *
   * @param values array of doubles.
   * @throws IllegalArgumentException All rows must have the same length
   */
  inline def copyFrom[M <: Int, N <: Int](values: NArray[Double])(using ValueOf[M], ValueOf[N]): Mat[M, N] = apply(narr.copy[Double](values))

  /**
   * Generates an MxN matrix which consists of elements randomized between [-1.0, 1.0] inclusive.
   *
   * @tparam N the number of columns
   * @return
   */
  inline def random[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]): Mat[M, N] = random[M, N](slash.Random.defaultRandom)

  /**
   * Generates an MxN matrix which consists of elements randomized between [-1.0, 1.0] inclusive.
   * @param r optional random instance.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with uniformly distributed random elements.
   */
  inline def random[M <: Int, N <: Int](r:scala.util.Random)(using ValueOf[M], ValueOf[N]):Mat[M, N] = {
    random(slash.interval.`[]`(-1.0, 1.0), r)
  }

  /** Generate matrix with random elements
   *
   * @param minNorm Minimum random generated value allowed, inclusive.
   * @param normMAX Maximum random generated value allowed, inclusive.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with uniformly distributed random elements.
   */
  inline def random[M <: Int, N <: Int](
    minNorm: Double,
    normMAX: Double
  )(using ValueOf[M], ValueOf[N]): Mat[M, N] = random[M, N](minNorm, normMAX, slash.Random.defaultRandom)


  /** Generate matrix with random elements
   * @param minNorm Minimum random generated value allowed, inclusive.
   * @param normMAX Maximum random generated value allowed, inclusive.
   * @param r optional random instance.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with uniformly distributed random elements.
   */
  inline def random[M <: Int, N <: Int](
    minNorm:Double,
    normMAX:Double,
    r:scala.util.Random
  )(using ValueOf[M], ValueOf[N]): Mat[M, N] = random[M, N](slash.interval.`[]`(minNorm, normMAX), r)

  /** Generate matrix with random elements
   *
   * @param interval from which to draw matrix component values.
   * @param r       optional random instance.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with uniformly distributed random elements.
   */
  def random[M <: Int, N <: Int](
    interval:slash.interval.Interval[Double],
    r: scala.util.Random
  )(using ValueOf[M], ValueOf[N]): Mat[M, N] = new Mat[M, N](
    MatrixData.tabulate(valueOf[M], valueOf[N], (_,_) => interval.random(r) )
  )

  /** Generate identity matrix
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with ones on the diagonal and zeros elsewhere.
   */
  def identity[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]): Mat[M, N] = diagonal[M, N](1.0)

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

  def diagonal[M <: Int, N <: Int](value:Double)(using ValueOf[M], ValueOf[N]): Mat[M, N] = new Mat[M,N](
    util.diagonal(valueOf[M], valueOf[N], value)
  )

  /**
   * Generate a square matrix with the supplied vector along the diagonal.
   * @param v a vector
   * @return
   */
  def diagonal[D <: Int](v:Vec[D])(using ValueOf[D]): Mat[D, D] = new Mat[D, D](util.diagonal(v.asNativeArray))

  /**
   * Create a rectangular matrix with the given vector along the diagonal.
   * @param v a vector to place along the diagonal.
   * @tparam M the number of rows.
   * @tparam N the number of rows.
   * @tparam D the dimension of the vector.  Must equal either M or N.
   * @return an MxN rectangular matrix with the given vector along the diagonal.
   */
  def diagonal[M <: Int, N <: Int, D <: Int](v: Vec[D])(using ValueOf[M], ValueOf[N]): Mat[M, N] = new Mat[M,N](
    util.diagonal(v.asNativeArray)
  )

  /** Construct a matrix from a 2-D array.
   *
   * @param arr2d Two-dimensional array of doubles.  arr2d(row)(column).
   */

  def apply[M <: Int, N <: Int](arr2d:NArray[Vec[N]])(using ValueOf[M], ValueOf[N]):Mat[M, N] = new Mat[M,N](
    new MatrixDataGrid(arr2d.asInstanceOf[NArray[NArray[Double]]])
  )

  /** Construct an MxN constant matrix.
   * @param value Fill the matrix with this scalar value.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return an MxN constant matrix.
   */
  def fill[M <: Int, N <: Int](value: Double)(using ValueOf[M], ValueOf[N]):Mat[M, N] = new Mat[M, N](
    util.fill(valueOf[M], valueOf[N], value)
  )

  /** Construct an MxN matrix of zeros.
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   */
  def zeros[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]):Mat[M, N] = new Mat[M, N](
    util.zeros(valueOf[M], valueOf[N])
  )

  /** Construct an MxN matrix of ones.
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   */
  def ones[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]): Mat[M, N] = new Mat[M, N](
    util.fill(valueOf[M], valueOf[N], 1.0)
  )

  /** Construct a matrix from a one-dimensional packed array
   *
   * @param values matrix data.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @throws IllegalArgumentException NArray length must equal M * N
   */
  /** Construct a matrix from a one-dimensional packed array
   *
   * @param values One-dimensional array of doubles, packed by rows.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @throws IllegalArgumentException NArray length must equal M * N
   */
  def apply[M <: Int, N <: Int](md: MatrixData)(using ValueOf[M], ValueOf[N]):Mat[M,N] = new Mat[M, N](md)

  /** Construct a matrix from a one-dimensional packed array
   *
   * @param values One-dimensional array of doubles, packed by rows.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @throws IllegalArgumentException NArray length must equal M * N
   */
  def apply[M <: Int, N <: Int](arr: NArray[Double] | NArray[NArray[Double]])(using ValueOf[M], ValueOf[N]):Mat[M,N] = {
    arr match {
      case values: NArray[Double] => new Mat[M, N](new LinearizedMatrixData(valueOf[M], valueOf[N], values))
      case values: NArray[NArray[Double]] => new Mat[M, N](new MatrixDataGrid(values))
      case _ => throw IllegalArgumentException("Expected either NArray[Double] or NArray[NArray[Double]].")
    }
  }


  /**
   *
   * @param values the matrix elements.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return an M x N matrix consisting of values.
   */
  def apply[M <: Int, N <: Int](values: Double *)(using ValueOf[M], ValueOf[N]):Mat[M, N] = {
    dimensionCheck(values.size, valueOf[M] * valueOf[N])
    new Mat[M, N](new LinearizedMatrixData(valueOf[M], valueOf[N], NArray[Double](values *)))
  }

  // support left multiply by scalar
  extension (d: Double) {
    def *[M <: Int, N <: Int](m: Mat[M,N]): Mat[M,N] = m * d // same as right multiply
  }

}

class Mat[M <: Int, N <: Int] private (val values: MatrixData)(using ValueOf[M], ValueOf[N]) {
  val rows: Int = valueOf[M]
  val columns: Int = valueOf[N]

  require(rows == values.rowDimension, s"Rows: $rows != ${values.rowDimension}")
  require(columns == values.columnDimension, s"Columns: $columns != ${values.columnDimension}")

  //inline def lindex(r:Int, c:Int):Int = (r * columns) + c

  /** Make a deep copy of a matrix
    */
  def copy: Mat[M, N] = new Mat[M, N](values.copy)

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
  inline def rowVector(row:Int):Vec[N] = Vec.apply[N](values.getRow(row))

  /**
   * @return a copy of this matrix in the form of an array of row vectors.
   */
  inline def rowVectors: NArray[Vec[N]] = util.as2DNArray(values).asInstanceOf[NArray[Vec[N]]]

  /**
   * @param column the column of the matrix to return as a vector.
   * @return a copy of the specified matrix column in Vec[M] format.
   */
  inline def columnVector(column:Int):Vec[M] =  util.extractColumn(column, values).asInstanceOf[Vec[M]]

  /**
   * @return a copy of this matrix in the form of an array of column vectors.
   */
  inline def columnVectors: NArray[Vec[M]] = transpose.rowVectors

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
  inline def apply(r: Int, c: Int): Double = values(r, c)

  /** Set a single element.
   *
   * @param r     Row index.
   * @param c     Column index.
   * @param value values(i,j).
   * @throws ArrayIndexOutOfBoundsException
   */
  inline def update(r: Int, c: Int, value: Double): Unit = values(r, c) = value

  /** Get a submatrix.
   *
   * @tparam M1 the number of rows
   * @tparam N1 the number of columns
   * @param r0 Initial row index
   * @param c0 Initial column index
   * @return A(i0:i1,j0:j1)
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def subMatrix[M1 <: Int, N1 <: Int](r0: Int, c0: Int)(using ValueOf[M1], ValueOf[N1]): Mat[M1, N1] = new Mat[M1, N1](
    util.subMatrix(values, r0, c0, valueOf[M1], valueOf[N1])
  )

  /** Get a submatrix.
    *
    * @param rowIndices Array of row indices.
    * @param columnIndices Array of column indices.
    * @return A(r(:),c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def subMatrix[M1 <: Int, N1 <: Int](
    rowIndices: NArray[Int], columnIndices: NArray[Int]
  )(using ValueOf[M1], ValueOf[N1]): Mat[M1, N1] = new Mat[M1, N1](
    util.subMatrix(values, rowIndices, columnIndices)
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
    new Mat[M1, N1](util.subMatrix(values, r0: Int, valueOf[M1], columnIndices: NArray[Int]))
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
    new Mat[M1, N1](util.subMatrix(values, rowIndices, valueOf[N1], c0))
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
      values, r0, c0, // original
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
    util.setMatrix(values, rowIndices, columnIndices, thatMatrix.values)
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
    util.setMatrix(values, rowIndices, c0, thatMatrix.values)
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
    util.setMatrix(values, r0, columnIndices, thatMatrix.values)
  }

  /**
   * Mat transpose.
   *
   * @return Mᵀ
   */
  def transpose: Mat[N, M] = new Mat[N, M](values.transpose)

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

  def + (B: Mat[M, N]): Mat[M, N] = {
    val out = copy
    out.add(B)
    out
  }

  inline def += (B: Mat[M, N]): Unit = add(B)

  /** A = A + B
    *
    * @param B another matrix
    * @return A + B
    */
  def add(B: Mat[M, N]): Unit = util.add(values, B.values)

  /** Unary minus
   *
   * @return -A
   */
  inline def unary_- : Mat[M, N] = * ( -1.0 )

  def - (B: Mat[M, N]): Mat[M, N] = {
    val out = copy
    out.subtract(B)
    out
  }

  inline def -= (B: Mat[M, N]): Unit = subtract(B)

  /** A = A - B
   *
   * @param B another matrix
   * @return A - B
   */
  def subtract(B: Mat[M, N]): Unit = util.subtract(values, B.values)

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
  def * (s: Double): Mat[M, N] = {
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
  def + (s: Double): Mat[M, N] = {
    val out = copy
    out.addScalar(s)
    out
  }

  inline def += (s:Double): Unit = addScalar(s)

  def - (s: Double): Mat[M, N] = {
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

  inline def * (v: slash.vectorf.VecF[N]): slash.vectorf.VecF[N] = times(v)

  inline def times (v: slash.vectorf.VecF[N]): slash.vectorf.VecF[N] = {
    util.times(values, v.asNativeArray).asInstanceOf[slash.vectorf.VecF[N]]
  }

  inline def * (v: Vec[N]): Vec[N] = times(v)

  inline def times (v: Vec[N]): Vec[N] = util.times(values, v.asNativeArray).asInstanceOf[Vec[N]]

  inline def * [V <: Int](thatMatrix: Mat[N, V])(using ValueOf[V]): Mat[M, V] = times(thatMatrix)

  /** Linear algebraic matrix multiplication, A * B
    *
    * @param b another matrix
    * @return Mat product, A * B
    * @throws IllegalArgumentException Mat inner dimensions must agree.
    */
  def times[V <: Int](b: Mat[N, V])(using ValueOf[V]): Mat[M, V] = new Mat[M, V](util.times(values, b.values))

  /** Pointwise multiplication (hadamard product) A * B
    *
    * @param B another matrix
    * @return A * B (where * is the Hadamard Product)
    */
  inline def pointwiseMultiply(B: Mat[M, N]): Unit = util.pointwiseMultiply(values, B.values)

  /** Pointwise multiplication (hadamard product) A * B
    *
    * @param B another matrix
    * @return A * B (where * is the Hadamard Product)
    */
  def pointwiseMultiplied(B: Mat[M, N]): Mat[M, N] = {
    val out = copy
    out.pointwiseMultiply(B)
    out
  }

  def kronecker[V <: Int, W <: Int](b: Mat[V, W])(using ValueOf[V * M], ValueOf[W * N]): Mat[V * M, W * N] = {
    new Mat[V * M, W * N](
      util.kronecker(values, b.values)
    )
  }

  /** Mat trace.
    *
    * @return sum of the diagonal elements.
    */
  inline def trace: Double = util.trace(values)

  inline def dim: String = s"dim(${rows}x$columns)"

  def concatenateRows[M1 <: Int](m: Mat[M1, N])(using ValueOf[+[M, M1]]): Mat[+[M, M1], N] = new Mat[+[M, M1], N](
    MatrixData.concatenateRows(values, m.values)
  )

//  inline def concatenateRows(m: Mat[? <: Int, ? <: Int]): NArray[Double] = util.concatenateRows(values, m.values)

  def concatenateColumns[N1 <: Int](m: Mat[M,N1])(using ValueOf[+[N,N1]]): Mat[M, +[N,N1]] = new Mat[M, +[N, N1]](
    MatrixData.concatenateColumns(values, m.values)
  )

//  inline def concatenateColumns(m: Mat[? <: Int, ? <: Int]): NArray[Double] = {
//    util.concatenateColumns(values, m.values)
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

  def upperTriangular: Mat[M,N] = new Mat[M, N](
    util.upperTriangular(values)
  )

  def lowerTriangular: Mat[M,N] = new Mat[M, N](
    util.lowerTriangular(values)
  )

  inline def diagonalVector: Vec[Min[M,N]] = {
    util.diagonalVector(values).asInstanceOf[Vec[Min[M,N]]]
  }

  def render(
    format: MatFormat = MatFormat.DEFAULT,
    alignment: Function2[MatrixData, MatFormat, NArray[NArray[String]]] = MatFormat.ALIGN_ON_DECIMAL,
    sb: StringBuilder = new StringBuilder()
  ): StringBuilder = {
    format.render(values, alignment, sb)
  }

  override def toString: String = csv

  def csv: String = csv(MatFormat.UNALIGNED, new StringBuilder()).toString

  def csv(
    alignment: Function2[MatrixData, MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): String = render(MatFormat.CSV, alignment, sb).toString

  def tsv: String = tsv(MatFormat.UNALIGNED, new StringBuilder()).toString

  def tsv(
    alignment: Function2[MatrixData, MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): String = render(MatFormat.TSV, alignment, sb).toString

}
