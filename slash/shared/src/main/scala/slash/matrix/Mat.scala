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
import slash.vector.*

import scala.compiletime.ops.int.*
import scala.compiletime.{constValue, error, summonFrom}
import scala.math.hypot

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

  /** Construct a matrix of random values.
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
    NArray.tabulate[Double]( valueOf[M] * valueOf[N] )( _ => interval.random(r) )
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
   * @return An MxN matrix with ones on the diagonal and zeros elsewhere.
   */

  def diagonal[M <: Int, N <: Int](value:Double)(using ValueOf[M], ValueOf[N]): Mat[M, N] = {
    val out:Mat[M, N] = zeros[M, N]
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
  def diagonal[D <: Int](v:Vec[D])(using ValueOf[D]): Mat[D, D] = {
    val out:Mat[D, D] = zeros[D, D]
    var i:Int = 0
    while (i < v.dimension) {
      out(i, i) = v(i)
      i = i + 1
    }
    out
  }

  def diagonal[M <: Int, N <: Int, D <: Int](v: Vec[D])(using ValueOf[M], ValueOf[N], ValueOf[D]): Mat[M, N] = {

    val rows:Int = valueOf[M]
    val columns:Int = valueOf[N]

    val out: Mat[M, N] = zeros[M, N]

    var i: Int = 0
    while (i < Math.min(valueOf[D], Math.min(rows, columns))) {
      out(i, i) = v(i)
      i = i + 1
    }
    out
  }

  /** Construct a matrix from a 2-D array.
   *
   * @param arr2d Two-dimensional array of doubles.  arr2d(row)(column).
   * @throws IllegalArgumentException All rows must have the same length
   */

  def apply[M <: Int, N <: Int](arr2d:NArray[Vec[N]])(using ValueOf[M], ValueOf[N]):Mat[M, N] = {
    val columns:Int = valueOf[N]
    var r:Int = 0; while (r < arr2d.length) {
      if (arr2d(r).dimension != columns) throw new IllegalArgumentException("Cannot create a Mat from a Jagged Array.")
      r += 1
    }

    val rows:Int = valueOf[M]
    val values:NArray[Double] = new NArray[Double](rows * columns)
    var i:Int = 0
    r = 0; while (r < rows) {
      var c:Int = 0; while (c < columns) {
        values(i) = arr2d(r)(c)
        i += 1
        c += 1
      }
      r += 1
    }
    new Mat[M, N](values)
  }


  /** Construct an MxN constant matrix.
   * @param value Fill the matrix with this scalar value.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return an MxN constant matrix.
   */
  def fill[M <: Int, N <: Int](value: Double)(using ValueOf[M], ValueOf[N]):Mat[M, N] = apply[M, N](
    NArray.fill[Double](valueOf[M] * valueOf[N])(value)
  )

  /** Construct an MxN matrix of zeros.
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   */
  def zeros[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]):Mat[M, N] = fill[M, N](0.0)

  /** Construct an MxN matrix of ones.
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   */
  def ones[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]): Mat[M, N] = fill[M, N](1.0)

  /** Construct a matrix from a one-dimensional packed array
   *
   * @param values One-dimensional array of doubles, packed by rows.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @throws IllegalArgumentException NArray length must equal M * N
   */
  def apply[M <: Int, N <: Int](values: NArray[Double])(using ValueOf[M], ValueOf[N]):Mat[M, N] = new Mat[M, N](values)

  /**
   *
   * @param values the matrix elements.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return an M x N matrix consisting of values.
   */
  def apply[M <: Int, N <: Int](values: Double *)(using ValueOf[M], ValueOf[N]):Mat[M, N] = {
    dimensionCheck(values.size, valueOf[M] * valueOf[N])
    new Mat[M, N](NArray[Double](values *))
  }

  // support left multiply by scalar
  extension (d: Double) {
    def *[M <: Int, N <: Int](m: Mat[M,N]): Mat[M,N] = m * d // same as right multiply
  }

}

class Mat[M <: Int, N <: Int](val values: NArray[Double])(using ValueOf[M], ValueOf[N]) {
  val rows: Int = valueOf[M]
  val columns: Int = valueOf[N]

  val MxN: Int = rows * columns
  opaque type MN <: Int = M * N

  require(rows * columns == values.length, s"Product of $rows x $columns != ${values.length}")

  inline def lindex(r:Int, c:Int):Int = (r * columns) + c

  /** Make a deep copy of a matrix
    */
  def copy: Mat[M, N] = new Mat[M, N](copyValues)

  /** Copy the internal two-dimensional array.
    *
    * @return Two-dimensional array copy of matrix elements.
    */
  inline def copyValues: NArray[Double] = narr.copy[Double](values)

  /** Make a one-dimensional column packed copy of the internal array.
    *
    * @return Mat elements packed in a one-dimensional array by columns.
    */
  def columnPackedArray: NArray[Double] = {
    val vs: NArray[Double] = new NArray[Double](rows * columns)
    var i:Int = 0; while (i < rows) {
      var j:Int = 0; while (j < columns) {
        vs(i + j*rows) = apply(i,j)
        j = j + 1
      }
      i = i + 1
    }
    vs
  }

  /** Make a one-dimensional row packed copy of the internal array.
    *
    * @return Mat elements packed in a one-dimensional array by rows.
    */
  inline def rowPackedArray: NArray[Double] = copyValues

  /**
   * @param row the row of the matrix to return as a vector.
   * @return a copy of the specified matrix row in Vec[N] format.
   */
  inline def rowVector(row:Int):Vec[N] = Vec.apply[N](
    values.asInstanceOf[NArr[Double]].slice(row * columns, (row * columns) + columns).asInstanceOf[NArray[Double]]
  )

  /**
   * @return a copy of this matrix in the form of an array of row vectors.
   */
  def rowVectors: NArray[Vec[N]] = NArray.tabulate[Vec[N]](rows)( (row: Int) => rowVector(row) )

  /**
   * @param column the column of the matrix to return as a vector.
   * @return a copy of the specified matrix column in Vec[M] format.
   */
  inline def columnVector(column:Int):Vec[M] = Vec.tabulate[M]( (r:Int) => apply(r, column) )

  /**
   * @return a copy of this matrix in the form of an array of column vectors.
   */
  def columnVectors: NArray[Vec[M]] = transpose.rowVectors


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
  inline def apply(r: Int, c: Int): Double = values(lindex(r, c))

  /** Set a single element.
   *
   * @param r     Row index.
   * @param c     Column index.
   * @param value values(i,j).
   * @throws ArrayIndexOutOfBoundsException
   */
  inline def update(r: Int, c: Int, value: Double): Unit = values(lindex(r, c)) = value

  /** Get a submatrix.
   *
   * @tparam M1 the number of rows
   * @tparam N1 the number of columns
   * @param r0 Initial row index
   * @param c0 Initial column index
   * @return A(i0:i1,j0:j1)
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def subMatrix[M1 <: Int, N1 <: Int](r0: Int, c0: Int)(using ValueOf[M1], ValueOf[N1]): Mat[M1, N1] = {

    val subRows:Int = valueOf[M1]
    val subColumns:Int = valueOf[N1]

    val rEnd:Int = r0 + subRows
    val cEnd:Int = c0 + subColumns

    val vs: NArray[Double] = new NArray[Double](subRows * subColumns)
    var i: Int = 0
    var r:Int = r0; while (r < rEnd) {
      var c: Int = c0; while (c < cEnd) {
        vs(i) = apply(r, c)
        i += 1
        c += 1
      }
      r += 1
    }
    new Mat[M1, N1](vs)

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
  )(using ValueOf[M1], ValueOf[N1]): Mat[M1, N1] = {
    val vs:NArray[Double] = new NArray[Double](rowIndices.length * columnIndices.length)
    var i:Int = 0
    var ri:Int = 0; while (ri < rowIndices.length) {
      var ci: Int = 0; while (ci < columnIndices.length) {
        vs(i) = apply( rowIndices(ri), columnIndices(ci) )
        i += 1
        ci += 1
      }
      ri += 1
    }
    new Mat[M1, N1](vs)
  }

  /** Get a submatrix.
    *
    * @param r0 Initial row index
    * @param columnIndices  Array of column indices.
    * @return A(i0:i1,c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def subMatrix[M1 <: Int, N1 <: Int](r0: Int, columnIndices: NArray[Int])(using ValueOf[M1], ValueOf[N1]): Mat[M1, N1] = {
    val subRows:Int = valueOf[M1]
    val subColumns:Int = valueOf[N1]
    val rEnd:Int = r0 + subRows

    val vs: NArray[Double] = new NArray[Double](subRows * subColumns)
    var i: Int = 0
    var ri: Int = r0; while (ri < rEnd) {
      var ci: Int = 0; while (ci < columnIndices.length) {
//        println(s"$ci ($ri, ${columnIndices(ci)}) and ${lindex(ri, columnIndices(ci))} of ${values.length}")
        vs(i) = apply(ri, columnIndices(ci))
        i += 1
        ci += 1
      }
      ri += 1
    }
    new Mat[M1, N1](vs)
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
    val subRows: Int = valueOf[M1]
    val subColumns: Int = valueOf[N1]
    val cEnd = c0 + subColumns
    val vs: NArray[Double] = new NArray[Double](subRows * subColumns)
    var i: Int = 0
    var ri: Int = 0; while (ri < rowIndices.length) {
      var ci: Int = c0; while (ci < cEnd) {
        vs(i) = apply( rowIndices(ri), ci )
        i += 1
        ci += 1
      }
      ri += 1
    }
    new Mat[M1, N1](vs)
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
    val rEnd: Int = valueOf[M1] + r0
    val cEnd: Int = valueOf[N1] + c0
    var r:Int = r0
    while (r < rEnd) {
      var c = c0
      while (c < cEnd) {
        update(r, c, thatMatrix(r-r0,c-c0))
        c = c + 1
      }
      r = r + 1
    }
  }

  /** Set a submatrix.
    *
    * @param rowIndices Array of row indices.
    * @param columnIndices Array of column indices.
    * @param thatMatrix A(r(:),c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], columnIndices: NArray[Int], thatMatrix: Mat[M1, N1]): Unit = {
    var i:Int = 0
    while (i < rowIndices.length) {
      var j:Int = 0; while (j < columnIndices.length) {
        update(rowIndices(i), columnIndices(j), thatMatrix(i, j))
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
    * @param thatMatrix  A(r(:),j0:j1)
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], c0: Int, thatMatrix: Mat[M1, N1])(using ValueOf[M1], ValueOf[N1]): Unit = {
    val c1:Int = c0 + valueOf[N1]
    var r:Int = 0
    while (r < rowIndices.length) {
      var c:Int = c0
      while (c < c1) {
        update(rowIndices(r), c, thatMatrix(r, c - c0))
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
    * @param thatMatrix  A(i0:i1,c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def setMatrix[M1 <: Int, N1 <: Int](r0: Int, columnIndices: NArray[Int], thatMatrix: Mat[M1, N1])(using ValueOf[M1], ValueOf[N1]): Unit = {
    val r1:Int = r0 + valueOf[M1]
    var r:Int = r0
    while (r < r1) {
      var c:Int = 0
      while (c < columnIndices.length) {
        update(r, columnIndices(c), thatMatrix(r - r0, c))
        c = c + 1
      }
      r = r + 1
    }
  }

  /** Mat transpose.
    *
    * @return Mᵀ
    */
  def transpose: Mat[N, M] = new Mat[N, M](columnPackedArray)

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
        columnSum += Math.abs(apply(r, c))
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
        rowSum += Math.abs(apply(r, c))
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
    var f:Double = 0.0

    var i:Int = 0
    while (i < values.length) {
      f = hypot(f, values(i))
      i = i + 1
    }

    f
  }

  inline def + (B: Mat[M, N]): Mat[M, N] = copy.add(B)

  inline def += (B: Mat[M, N]): Mat[M, N] = add(B)

  /** A = A + B
    *
    * @param B another matrix
    * @return A + B
    */
  def add(B: Mat[M, N]): Mat[M, N] = {
    var v:Int = 0
    while (v < values.length) {
      values(v) = values(v) + B.values(v)
      v = v + 1
    }
    this
  }

  /** Unary minus
   *
   * @return -A
   */
  inline def unary_- : Mat[M, N] = * ( -1.0 )

  inline def - (B: Mat[M, N]): Mat[M, N] = copy.subtract(B)

  inline def -= (B: Mat[M, N]): Mat[M, N] = subtract(B)

  /** A = A - B
    *
    * @param B another matrix
    * @return A - B
    */
  def subtract(B: Mat[M, N]): Mat[M, N] = {
    var v:Int = 0
    while (v < values.length) {
      values(v) = values(v) - B.values(v)
      v = v + 1
    }
    this
  }

  /** A = A + d
  *
  * @param d a scalar
  * @return A + d
  */
  def addScalar(d: Double)(using ValueOf[N]): Mat[M,N] = {
    var i:Int = 0
    while(i < values.length){
      values(i) += d
      i += 1
    }
    this
  }

  /** Multiply a matrix by a scalar, C = A * s
    *
    * @param s scalar
    * @return A * s
    */
  inline def * (s: Double): Mat[M, N] = copy.times(s)

  inline def *= (s:Double):Mat[M, N] = times(s)

  /** Add a scalar to a matrix, C = A + s
    *
    * @param s scalar
    * @return A + s
    */
  inline def + (s: Double): Mat[M, N] = copy.addScalar(s)

  inline def += (s:Double):Mat[M, N] = addScalar(s)

  inline def - (s: Double): Mat[M, N] = copy.addScalar(-s)

  inline def -= (s: Double): Mat[M, N] = addScalar(-s)

  /** Multiply a matrix by a scalar in place, A = s*A
    *
    * @param s scalar
    * @return replace A by s*A
    */
  def times(s: Double): Mat[M, N] = {
    var i:Int = 0
    while (i < values.length) {
      values(i) = values(i) * s
      i += 1
    }
    this
  }


  inline def * (v: slash.vectorf.VecF[N])(using ValueOf[N]): slash.vectorf.VecF[N] = times(v)

  def times (v: slash.vectorf.VecF[N])(using ValueOf[N]): slash.vectorf.VecF[N] = {
    val a:NArray[Float] = NArray.ofSize[Float](v.dimension)
    var i:Int = 0
    var ai:Int = 0
    while (i < values.length) {
      var dotProduct = 0.0
      var offset: Int = 0
      while (offset < v.dimension) {
        dotProduct = dotProduct + (values(i + offset) * v(offset))
        offset = offset + 1
      }
      a(ai) = dotProduct.toFloat
      ai = ai + 1
      i = i + v.dimension
    }
    a.asInstanceOf[slash.vectorf.VecF[N]]
  }

  inline def * (v: Vec[N])(using ValueOf[N]): Vec[N] = times(v)

  def times (v: Vec[N])(using ValueOf[N]): Vec[N] = {
    val a:NArray[Double] = NArray.ofSize[Double](v.dimension)
    var i:Int = 0
    var ai:Int = 0
    while (i < values.length) {
      var dotProduct = 0.0
      var offset: Int = 0
      while (offset < v.dimension) {
        dotProduct = dotProduct + (values(i + offset) * v(offset))
        offset = offset + 1
      }
      a(ai) = dotProduct
      ai = ai + 1
      i = i + v.dimension
    }
    a.asInstanceOf[Vec[N]]
  }

  def * [V <: Int](thatMatrix: Mat[N, V])(using ValueOf[V]): Mat[M, V] = times(thatMatrix)

  /** Linear algebraic matrix multiplication, A * B
    *
    * @param b another matrix
    * @return Mat product, A * B
    * @throws IllegalArgumentException Mat inner dimensions must agree.
    */
  def times[V <: Int](b: Mat[N, V])(using ValueOf[V]): Mat[M, V] = {

    val X:Mat[M, V] = Mat.zeros[M, V]

    val Bcolj = new NArray[Double](columns)

    var j:Int = 0
    while (j < b.columns) {
      var k:Int = 0
      while (k < columns) {
        Bcolj(k) = b(k, j)
        k = k + 1
      }
      var i:Int = 0
      while (i < rows) {
        var s:Double = 0.0
        k = 0
        while (k < columns) {
          s += apply(i, k) * Bcolj(k)
          k = k + 1
        }
        X(i, j) = s
        i = i + 1
      }
      j = j + 1
    }
    X
  }

  /** Hadamard product (element wise multiplication) A * B
    *
    * @param B another matrix
    * @return A * B (where * is the Hadamard Product)
    */
  def hadamard(B: Mat[M, N]): Mat[M, N] = {
    var v:Int = 0
    while (v < values.length) {
      values(v) = values(v) * B.values(v)
      v = v + 1
    }
    this
  }

  def kronecker[V <: Int, W <: Int](b: Mat[V, W])(using ValueOf[V * M], ValueOf[W * N]): Mat[V * M, W * N] = {
    val X: Mat[V * M, W * N] = Mat.zeros[V * M, W * N]

    var i1: Int = 0; while (i1 < rows) {
      val iBase = i1 * rows
      var j1: Int = 0; while (j1 < columns) {
        val jBase = j1 * columns
        val k = apply(i1, j1)
        var i2: Int = 0; while (i2 < b.rows) {
          var j2: Int = 0; while (j2 < b.columns) {
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

  /** Mat trace.
    *
    * @return sum of the diagonal elements.
    */
  def trace: Double = {
    var t = 0.0
    var i:Int = 0
    while (i < Math.min(rows, columns)) {
      t += apply(i, i)
      i = i + 1
    }
    t
  }

  def dim: String = s"dim(${rows}x$columns)"

  inline def concatenateRows[M1 <: Int](m: Mat[M1, N])(using ValueOf[M1], ValueOf[+[M, M1]]): Mat[+[M, M1], N] = {
    val out:NArray[Double] = new NArray[Double](values.length + m.values.length)
    narr.native.NArray.copyDoubleArray( values, 0, out, 0, values.length )
    narr.native.NArray.copyDoubleArray( m.values, 0, out, values.length, m.values.length)
    new Mat[+[M, M1], N](out)
  }

  inline def concatenateRows(m: Mat[? <: Int, ? <: Int]): NArray[Double] = {
    val out: NArray[Double] = new NArray[Double](values.length + m.values.length)
    narr.native.NArray.copyDoubleArray(values, 0, out, 0, values.length)
    narr.native.NArray.copyDoubleArray(m.values, 0, out, values.length, m.values.length)
    out
  }

  inline def concatenateColumns[N1 <: Int](m: Mat[M,N1])(using ValueOf[N1], ValueOf[+[N,N1]]): Mat[M, +[N,N1]] = {
    val out:NArray[Double] = new NArray[Double](values.length + m.values.length)
    var r:Int = 0
    var i:Int = 0
    while (r < rows) {
      narr.native.NArray.copyDoubleArray( values, r * columns, out, i, columns )
      i = i + columns
      narr.native.NArray.copyDoubleArray( m.values, r * m.columns, out, i, m.columns )
      i = i + m.columns
      r = r + 1
    }
    new Mat[M, +[N, N1]](out)
  }

  inline def concatenateColumns(m: Mat[? <: Int, ? <: Int]): NArray[Double] = {
    val out:NArray[Double] = new NArray[Double](values.length + m.values.length)
    var r:Int = 0
    var i:Int = 0
    while (r < rows) {
      narr.native.NArray.copyDoubleArray( values, r * columns, out, i, columns )
      i = i + columns
      narr.native.NArray.copyDoubleArray( m.values, r * m.columns, out, i, m.columns )
      i = i + m.columns
      r = r + 1
    }
    out
  }

  def asNativeArray2D: NArray[NArray[Double]] = rowVectors.asInstanceOf[NArray[NArray[Double]]]

  def strictEquals(obj: Any): Boolean = {
    obj match {
      case that: Mat[?, ?] =>
        var i: Int = 0
        var same: Boolean = this.MxN == that.MxN && this.rows == that.rows
        while (i < this.MxN && same) {
          inline def thisi = this.values(i)

          inline def thati = that.values(i)

          same &&= (thisi == thati) // || (thisi.isNaN && thati.isNaN)
          i += 1
        }
        same
      case _ => false
    }
  }

  def render(
    format: MatFormat = MatFormat.DEFAULT,
    alignment: Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): StringBuilder = {
    format.render(this, alignment, sb)
  }

  override def toString: String = csv

  def csv: String = csv(MatFormat.UNALIGNED, new StringBuilder()).toString

  def csv(
    alignment: Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): String = render(MatFormat.CSV, alignment, sb).toString

  def tsv: String = tsv(MatFormat.UNALIGNED, new StringBuilder()).toString

  def tsv(
    alignment: Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): String = render(MatFormat.TSV, alignment, sb).toString

  def upperTriangular: Mat[M,N] = {
    val out:Mat[M, N] = copy
    var i = 0
    while(i < rows){
      var j = 0
      while(j < i){
        out(i,j) = 0.0
        j += 1
      }
      i += 1
    }
    out
  }

  def lowerTriangular: Mat[M,N] = {
    val out:Mat[M, N] = copy
    var i = 0
    while(i < rows){
      var j = i+1
      while(j < columns){
        out(i,j) = 0.0
        j += 1
      }
      i += 1
    }
    out
  }

  def diagonalVector(using ValueOf[Min[M,N]]): Vec[Min[M,N]] = {
    val dim: Int = valueOf[M].min(valueOf[N])
    val arr: Vec[Min[M,N]] = new DoubleArray(dim).asInstanceOf[Vec[Min[M,N]]]
    var i = 0
    while(i < dim) {
      arr(i) = apply(i,i)
      i += 1
    }
    arr
  }

}

case class MatColumnMetrics(leftLength: NArray[Int], rightLength: NArray[Int], maxLength: NArray[Int])

trait MatFormat {
  def prefix[M <: Int, N <: Int](m: Mat[M, N]): String

  def rowPrefix[M <: Int, N <: Int](m: Mat[M, N]): String

  def delimiter[M <: Int, N <: Int](m: Mat[M, N])(r: Int, c: Int): String

  def suffix[M <: Int, N <: Int](m: Mat[M, N]): String

  def rowSuffix[M <: Int, N <: Int](m: Mat[M, N]): String

  def format(d: Double): String = d.toString

  def render[M <: Int, N <: Int](
    m: Mat[M, N],
    alignment: Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): StringBuilder = {

    val aligned:NArray[NArray[String]] = alignment(m, this)

    val dlm = delimiter(m)
    sb.append(prefix(m))

    var r = 0
    while (r < m.rows) {
      sb.append(rowPrefix(m))
      var c = 0
      while (c < m.columns) {
        sb.append(aligned(r)(c))
        sb.append(dlm(r, c))
        c = c + 1
      }
      sb.append(rowSuffix(m)).append("\n")
      r = r + 1
    }
    sb.append(suffix(m))
  }

  def columnMetrics(m: Mat[? <: Int, ? <: Int]): MatColumnMetrics = {
    val leftLength: NArray[Int] = NArray.fill[Int](m.columns)(0)
    val rightLength: NArray[Int] = NArray.fill[Int](m.columns)(0)
    val maxLength: NArray[Int] = NArray.fill[Int](m.columns)(0)

    var r = 0
    while (r < m.rows) {
      var c = 0
      while (c < m.columns) {
        val s = format(m(r, c))
        val parts = s.split('.')

        leftLength(c) = Math.max(leftLength(c), parts(0).length)
        rightLength(c) = Math.max(rightLength(c), parts(1).length)
        maxLength(c) = Math.max(maxLength(c), s.length)

        c = c + 1
      }
      r = r + 1
    }
    MatColumnMetrics(leftLength, rightLength, maxLength)
  }
}

object MatFormat {

  import slash.unicode.*

  val UNALIGNED: Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]] = (m: Mat[? <: Int, ? <: Int], fmt: MatFormat) => {
    NArray.tabulate[NArray[String]](m.rows)((r: Int) => NArray.tabulate[String](m.columns)((c: Int) => fmt.format(m(r, c))))
  }

  val ALIGN_LEFT: Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]] = (m:Mat[? <: Int, ? <: Int], fmt: MatFormat) => {
    val mcms:MatColumnMetrics = fmt.columnMetrics(m)
    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](m.rows)( (r: Int) => {
      NArray.tabulate[String](m.columns)((c: Int) => {
        val value = m(r, c)
        var s = fmt.format(value)
        while (s.length < mcms.maxLength(c)) s = s + " "
        s
      })
    })
    out
  }

  val ALIGN_RIGHT: Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]] = (m:Mat[? <: Int, ? <: Int], fmt: MatFormat) => {
    val mcms:MatColumnMetrics = fmt.columnMetrics(m)
    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](m.rows)( (r: Int) => {
      NArray.tabulate[String](m.columns)((c: Int) => {
        val value = m(r, c)
        var s = fmt.format(value)
        while (s.length < mcms.maxLength(c)) s = " " + s
        s
      })
    })
    out
  }

  val ALIGN_CENTER: Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]] = (m:Mat[? <: Int, ? <: Int], fmt: MatFormat) => {
    val mcms:MatColumnMetrics = fmt.columnMetrics(m)
    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](m.rows)( (r: Int) => {
      NArray.tabulate[String](m.columns)((c: Int) => {
        val value = m(r, c)
        var s = fmt.format(value)
        while (s.length < mcms.maxLength(c)) {
          s = " " + s
          if (s.length < mcms.maxLength(c)) s = s + " "
        }
        s
      })
    })
    out
  }

  val ALIGN_ON_DECIMAL: Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]] = (m:Mat[? <: Int, ? <: Int], fmt: MatFormat) => {
    val mcms:MatColumnMetrics = fmt.columnMetrics(m)
    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](m.rows)( (r: Int) => {
      NArray.tabulate[String](m.columns)((c: Int) => {
        val value = m(r, c)
        val parts = fmt.format(value).split('.')
        while (parts(0).length < mcms.leftLength(c)) parts(0) = " " + parts(0)
        while (parts(1).length < mcms.rightLength(c)) parts(1) = parts(1) + " "
        parts(0) + "." + parts(1)
      })
    })
    out
  }
  
//  val ALIGN_ON_MAGNITUDE: Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]] = (m:Mat[? <: Int, ? <: Int], fmt: MatFormat) => {
//    val mcms:MatColumnMetrics = fmt.columnMetrics(m)
//    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](m.rows)( (r: Int) => {
//      NArray.tabulate[String](m.columns)((c: Int) => {
//        val value = m(r, c)
//        var s = fmt.format(value)
//        //        val xpnnt: Int = slash.native.getExponent(value)
//        //        if (xpnnt > 0) {
//        //          val parts = s.split('.')
//        //          while (parts(0).length < mcms.leftLength(c)) parts(0) = " " + parts(0)
//        //          while (parts(1).length < mcms.rightLength(c)) parts(1) = parts(1) + " "
//        //          s = parts(0) + "." + parts(1)
//        //        } else if (xpnnt < 0) {
//        //          val parts = s.split('.')
//        //          while (parts(0).length < mcms.leftLength(c)) parts(0) = " " + parts(0)
//        //          while (parts(1).length < mcms.rightLength(c)) parts(1) = parts(1) + " "
//        //          s = parts(0) + "." + parts(1)
//        //        } else {
//        val parts = s.split('.')
//        while (parts(0).length < mcms.leftLength(c)) parts(0) = " " + parts(0)
//        while (parts(1).length < mcms.rightLength(c)) parts(1) = parts(1) + " "
//        s = parts(0) + "." + parts(1)
//        //        }
//        s
//      })
//    })
//    out
//  }

  object DEFAULT extends MatFormat {
    override def prefix[M <: Int, N <: Int](m: Mat[M, N]): String = s"Mat[${m.rows}, ${m.columns}](\n"

    override def delimiter[M <: Int, N <: Int](m: Mat[M, N])(r: Int, c: Int): String = {
      if (c == m.columns - 1) {
        if (r == m.rows - 1) "" else ","
      } else ", "
    }

    override def suffix[M <: Int, N <: Int](m: Mat[M, N]): String = ")\n"

    override def rowPrefix[M <: Int, N <: Int](m: Mat[M, N]): String = "  "

    override def rowSuffix[M <: Int, N <: Int](m: Mat[M, N]): String = ""
  }

  object TUPLE extends MatFormat {
    override def prefix[M <: Int, N <: Int](m: Mat[M, N]): String = s"Mat(\n"

    override def delimiter[M <: Int, N <: Int](m: Mat[M, N])(r: Int, c: Int): String = {
      if (c == m.columns - 1) {
        if (r == m.rows - 1) ")" else "),"
      } else ", "
    }

    override def suffix[M <: Int, N <: Int](m: Mat[M, N]): String = ")\n"

    override def rowPrefix[M <: Int, N <: Int](m: Mat[M, N]): String = "  ("

    override def rowSuffix[M <: Int, N <: Int](m: Mat[M, N]): String = ""
  }

  object TEXTBOOK extends MatFormat {
    // Mat₍₃ₓ₅₎
    override def prefix[M <: Int, N <: Int](m: Mat[M, N]): String = s"Mat${abase(m.rows)}ₓ${abase(m.columns)}\n"

    override def delimiter[M <: Int, N <: Int](m: Mat[M, N])(r: Int, c: Int): String = " "

    override def suffix[M <: Int, N <: Int](m: Mat[M, N]): String = "\n"

    override def rowPrefix[M <: Int, N <: Int](m: Mat[M, N]): String = "│  "

    override def rowSuffix[M <: Int, N <: Int](m: Mat[M, N]): String = " │\n"

    override def render[M <: Int, N <: Int](
      m: Mat[M, N],
      alignment: Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]],
      sb: StringBuilder = new StringBuilder()
    ): StringBuilder = {

      val aligned: NArray[NArray[String]] = alignment(m, this)

      val dlm = delimiter(m)
      sb.append(prefix(m))

      val firstRow: StringBuilder = new StringBuilder()
      var i:Int = 0
      while (i < m.columns) {
        firstRow.append(aligned(0)(i))
        firstRow.append(dlm(0, i))
        i = i + 1
      }

      sb.append("┌ ")
      i = 0
      while (i < firstRow.length) {
        sb.append(" ")
        i = i + 1
      }
      sb.append("  ┐\n")

      sb.append(rowPrefix(m)).append(s"$firstRow").append(rowSuffix(m))
      var r = 1
      while (r < m.rows) {
        sb.append(rowPrefix(m))
        var c = 0
        while (c < m.columns) {
          sb.append(aligned(r)(c))
          sb.append(dlm(r, c))
          c = c + 1
        }
        sb.append(rowSuffix(m))
        r = r + 1
      }
      sb.append("└ ")

      i = 0
      while (i < firstRow.length) {
        sb.append(" ")
        i = i + 1
      }
      sb.append("  ┘")
      sb.append(suffix(m))
    }
  }

  object INDEXED extends MatFormat {
    override def prefix[M <: Int, N <: Int](m: Mat[M, N]): String = s"Mat[${m.rows}x${m.columns}]\n"

    override def suffix[M <: Int, N <: Int](m: Mat[M, N]): String = ""

    override def delimiter[M <: Int, N <: Int](m: Mat[M, N])(r: Int, c: Int): String = {
      val maxRowDigits = m.rows.toString.length
      val maxColDigits = m.columns.toString.length
      var rs = abase(r + 1)
      while (rs.length < maxRowDigits) rs = "₀" + rs
      var cs = abase(c + 1)
      while (cs.length < maxColDigits) cs = "₀" + cs
      s"$rs,$cs "
    }

    override def rowPrefix[M <: Int, N <: Int](m: Mat[M, N]): String = "│  "

    override def rowSuffix[M <: Int, N <: Int](m: Mat[M, N]): String = "│"
  }

  case class Delimited(delimeter: String) extends MatFormat {
    override def prefix[M <: Int, N <: Int](m: Mat[M, N]): String = ""

    override def suffix[M <: Int, N <: Int](m: Mat[M, N]): String = ""

    override def delimiter[M <: Int, N <: Int](m: Mat[M, N])(r: Int, c: Int): String = {
      if (c == m.columns - 1) "" else s"$delimeter "
    }

    override def rowPrefix[M <: Int, N <: Int](m: Mat[M, N]): String = ""

    override def rowSuffix[M <: Int, N <: Int](m: Mat[M, N]): String = ""
  }

  lazy val CSV: MatFormat = Delimited(",")

  lazy val TSV: MatFormat = Delimited("\t")


  object ASCII extends MatFormat {
    override def prefix[M <: Int, N <: Int](m: Mat[M, N]): String = ""

    override def delimiter[M <: Int, N <: Int](m: Mat[M, N])(r: Int, c: Int): String = if (c == m.columns - 1) "" else ", "

    override def suffix[M <: Int, N <: Int](m: Mat[M, N]): String = ""

    override def rowPrefix[M <: Int, N <: Int](m: Mat[M, N]): String = "| "

    override def rowSuffix[M <: Int, N <: Int](m: Mat[M, N]): String = " |"
  }

  def main(args: Array[String]): Unit = {
    println("Matrix Printing Demo")

    import slash.Random.*
    val r: scala.util.Random = defaultRandom

    val m = r.nextMatrix[10, 15](Short.MinValue.toDouble, Short.MaxValue.toDouble)
    //val m = r.nextMatrix[10, 10](Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.MxN)) = Double.MinPositiveValue
    m.values(r.nextInt(m.MxN)) = Math.random()
    m.values(r.nextInt(m.MxN)) = Math.random()
    m.values(r.nextInt(m.MxN)) = Math.random() / 9875379845.0
    m.values(r.nextInt(m.MxN)) = Math.random() / 9875379845.0
    m.values(r.nextInt(m.MxN)) = Math.random() / 9875379845.0
    m.values(r.nextInt(m.MxN)) = r.between(Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.MxN)) = r.between(Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.MxN)) = r.between(Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.MxN)) = r.between(Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.MxN)) = Double.MaxValue



    val alignments:Array[(String, Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]])] = {
      Array[(String, Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]])](
        ("MatFormat.ALIGN_RIGHT", MatFormat.ALIGN_RIGHT),
        ("MatFormat.ALIGN_LEFT", MatFormat.ALIGN_LEFT),
        ("MatFormat.ALIGN_CENTER", MatFormat.ALIGN_CENTER),
        ("MatFormat.ALIGN_ON_DECIMAL", MatFormat.ALIGN_ON_DECIMAL)
      )
    }

    println("Unaligned CSV:")
    println(m.csv)
    println("Unaligned TSV:")
    println(m.tsv)

    for (a <- alignments) {
      println(s"\n/******** ${a._1} ********/\n")
      println("DEFAULT:")
      println(m.render(alignment = a._2))
      println("TUPLE:")
      println(m.render(TUPLE, a._2))
      println("TEXTBOOK:")
      println(m.render(TEXTBOOK, a._2))
      println("CSV:")
      println(m.csv(a._2))
      println("TSV:")
      println(m.tsv(a._2))
      println("ASCII:")
      println(m.render(ASCII, a._2))
      println("INDEXED:")
      println(m.render(INDEXED, a._2))
    }
  }

}
