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

package slash.matrixf

import narr.*
import slash.*
import vectorf.VecF

import scala.compiletime.ops.int.*
import scala.compiletime.{constValue, error, summonFrom}
import scala.math.hypot

/**
  * This library is fundamentally an adaptation of the Java MatF library, JaMa, by MathWorks Inc. and the National Institute of Standards and Technology.
  */

object MatF {
  /** Construct a matrix from a copy of an array.
   *
   * @param values array of Floats.
   * @throws IllegalArgumentException All rows must have the same length
   */
  inline def copyFrom[M <: Int, N <: Int](values: NArray[Float])(using ValueOf[M], ValueOf[N]): MatF[M, N] = apply(narr.copy[Float](values))

  /** Construct a matrix of random values.
   */
  inline def random[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]): MatF[M, N] = random[M, N](slash.Random.defaultRandom)

  /**
   * Generates an MxN matrix which consists of elements randomized between [-1.0, 1.0] inclusive.
   * @param r optional random instance.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with uniformly distributed random elements.
   */
  inline def random[M <: Int, N <: Int](r:scala.util.Random)(using ValueOf[M], ValueOf[N]):MatF[M, N] = {
    random(slash.interval.`[]`(-1.0f, 1.0f), r)
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
    minNorm: Float,
    normMAX: Float
  )(using ValueOf[M], ValueOf[N]): MatF[M, N] = random[M, N](minNorm, normMAX, slash.Random.defaultRandom)


  /** Generate matrix with random elements
   * @param minNorm Minimum random generated value allowed, inclusive.
   * @param normMAX Maximum random generated value allowed, inclusive.
   * @param r optional random instance.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with uniformly distributed random elements.
   */
  inline def random[M <: Int, N <: Int](
    minNorm:Float,
    normMAX:Float,
    r:scala.util.Random
  )(using ValueOf[M], ValueOf[N]): MatF[M, N] = random[M, N](slash.interval.`[]`(minNorm, normMAX), r)

  /** Generate matrix with random elements
   *
   * @param interval from which to draw matrix component values.
   * @param r       optional random instance.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with uniformly distributed random elements.
   */
  def random[M <: Int, N <: Int](
    interval:slash.interval.FloatInterval,
    r: scala.util.Random
  )(using ValueOf[M], ValueOf[N]): MatF[M, N] = new MatF[M, N](
    NArray.tabulate[Float]( valueOf[M] * valueOf[N] )( _ => interval.random(r) )
  )

  /** Generate identity matrix
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with ones on the diagonal and zeros elsewhere.
   */
  def identity[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]): MatF[M, N] = diagonal[M, N](1.0)


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

  def diagonal[M <: Int, N <: Int](value:Float)(using ValueOf[M], ValueOf[N]): MatF[M, N] = {
    val out:MatF[M, N] = zeros[M, N]
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
  def diagonal[D <: Int](v:VecF[D])(using ValueOf[D]): MatF[D, D] = {
    val out:MatF[D, D] = zeros[D, D]
    var i:Int = 0
    while (i < v.dimension) {
      out(i, i) = v(i)
      i = i + 1
    }
    out
  }

  def diagonal[M <: Int, N <: Int, D <: Int](v: VecF[D])(using ValueOf[M], ValueOf[N], ValueOf[D]): MatF[M, N] = {

    val rows:Int = valueOf[M]
    val columns:Int = valueOf[N]

    val out: MatF[M, N] = zeros[M, N]

    var i: Int = 0
    while (i < Math.min(valueOf[D], Math.min(rows, columns))) {
      out(i, i) = v(i)
      i = i + 1
    }
    out
  }

  /** Construct an MxN constant matrix.
   * @param value Fill the matrix with this scalar value.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return an MxN constant matrix.
   */
  def fill[M <: Int, N <: Int](value: Float)(using ValueOf[M], ValueOf[N]):MatF[M, N] = apply[M, N](
    NArray.fill[Float](valueOf[M] * valueOf[N])(value)
  )

  /** Construct an MxN matrix of zeros.
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   */
  def zeros[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]):MatF[M, N] = fill[M, N](0.0)

  /** Construct an MxN matrix of ones.
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   */
  def ones[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]): MatF[M, N] = fill[M, N](1.0)


  /** Construct a matrix from a one-dimensional packed array
   *
   * @param values One-dimensional array of Floats, packed by rows.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @throws IllegalArgumentException NArray length must equal M * N
   */
  def apply[M <: Int, N <: Int](values: NArray[Float])(using ValueOf[M], ValueOf[N]):MatF[M, N] = new MatF[M, N](values)

  /**
   *
   * @param values the matrix elements.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return an M x N matrix consisting of values.
   */
  def apply[M <: Int, N <: Int](values: Float *)(using ValueOf[M], ValueOf[N]):MatF[M, N] = {
    dimensionCheck(values.size, valueOf[M] * valueOf[N])
    new MatF[M, N](NArray[Float](values *))
  }

  /** Construct a matrix from a 2-D array.
   *
   * @param arr2d Two-dimensional array of Floats.  arr2d(row)(column).
   * @throws IllegalArgumentException All rows must have the same length
   */

  def apply[M <: Int, N <: Int](arr2d: NArray[VecF[N]])(using ValueOf[M], ValueOf[N]): MatF[M, N] = {
    val columns: Int = valueOf[N]
    var r: Int = 0;
    while (r < arr2d.length) {
      if (arr2d(r).dimension != columns) throw new IllegalArgumentException("Cannot create a MatF from a Jagged Array.")
      r += 1
    }

    val rows: Int = valueOf[M]
    val values: NArray[Float] = new NArray[Float](rows * columns)
    var i: Int = 0
    r = 0;
    while (r < rows) {
      var c: Int = 0;
      while (c < columns) {
        values(i) = arr2d(r)(c)
        i += 1
        c += 1
      }
      r += 1
    }
    new MatF[M, N](values)
  }

  /**
   * @param m an instance of slash.matrix.Mat[M, N]
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return an M x N matrix; an instance of slash.matrixf.MatF[M, N]
   */
  def fromMat[M <: Int, N <: Int](m:slash.matrix.Mat[M, N])(using ValueOf[M], ValueOf[N]): MatF[M, N] = {
    apply[M, N](NArray.tabulate[Float](valueOf[M] * valueOf[N])((i:Int) => m.values(i).toFloat))
  }

  // support left multiply by scalar
  extension (d: Float) {
    def *[M <: Int, N <: Int](m: MatF[M,N]): MatF[M,N] = m * d // same as right multiply
  }

}

class MatF[M <: Int, N <: Int](val values: NArray[Float])(using ValueOf[M], ValueOf[N]) {
  val rows: Int = valueOf[M]
  val columns: Int = valueOf[N]

  val MxN: Int = rows * columns
  opaque type MN <: Int = M * N

  require(rows * columns == values.length, s"Product of $rows x $columns != ${values.length}")

  inline def lindex(r:Int, c:Int):Int = (r * columns) + c

  /** Make a deep copy of a matrix
    */
  def copy: MatF[M, N] = new MatF[M, N](copyValues)

  /** Copy the internal two-dimensional array.
    *
    * @return Two-dimensional array copy of matrix elements.
    */
  inline def copyValues: NArray[Float] = narr.copy[Float](values)

  /** Make a one-dimensional column packed copy of the internal array.
    *
    * @return MatF elements packed in a one-dimensional array by columns.
    */
  def columnPackedArray: NArray[Float] = {
    val vs: NArray[Float] = new NArray[Float](rows * columns)
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
    * @return MatF elements packed in a one-dimensional array by rows.
    */
  inline def rowPackedArray: NArray[Float] = copyValues

  /**
   * @param row the row of the matrix to return as a vector.
   * @return a copy of the specified matrix row in VecF[N] format.
   */
  inline def rowVector(row:Int):VecF[N] = VecF.apply[N](
    values.asInstanceOf[NArr[Float]].slice(row * columns, (row * columns) + columns).asInstanceOf[NArray[Float]]
  )

  /**
   * @return a copy of this matrix in the form of an array of row vectors.
   */
  def rowVectors: NArray[VecF[N]] = NArray.tabulate[VecF[N]](rows)( (row: Int) => rowVector(row) )

  /**
   * @param column the column of the matrix to return as a vector.
   * @return a copy of the specified matrix column in VecF[M] format.
   */
  inline def columnVector(column:Int):VecF[M] = VecF.tabulate[M]( (r:Int) => apply(r, column) )

  /**
   * @return a copy of this matrix in the form of an array of column vectors.
   */
  def columnVectors: NArray[VecF[M]] = transpose.rowVectors


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
  inline def apply(r: Int, c: Int): Float = values(lindex(r, c))

  /** Set a single element.
   *
   * @param r     Row index.
   * @param c     Column index.
   * @param value values(i,j).
   * @throws ArrayIndexOutOfBoundsException
   */
  inline def update(r: Int, c: Int, value: Float): Unit = values(lindex(r, c)) = value

  /** Get a submatrix.
   *
   * @tparam M1 the number of rows
   * @tparam N1 the number of columns
   * @param r0 Initial row index
   * @param c0 Initial column index
   * @return A(i0:i1,j0:j1)
   * @throws ArrayIndexOutOfBoundsException Submatrix indices
   */
  def subMatrix[M1 <: Int, N1 <: Int](r0: Int, c0: Int)(using ValueOf[M1], ValueOf[N1]): MatF[M1, N1] = {

    val subRows:Int = valueOf[M1]
    val subColumns:Int = valueOf[N1]

    val rEnd:Int = r0 + subRows
    val cEnd:Int = c0 + subColumns

    val vs: NArray[Float] = new NArray[Float](subRows * subColumns)
    var i: Int = 0
    var r:Int = r0; while (r < rEnd) {
      var c: Int = c0; while (c < cEnd) {
        vs(i) = apply(r, c)
        i += 1
        c += 1
      }
      r += 1
    }
    new MatF[M1, N1](vs)

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
  )(using ValueOf[M1], ValueOf[N1]): MatF[M1, N1] = {
    val vs:NArray[Float] = new NArray[Float](rowIndices.length * columnIndices.length)
    var i:Int = 0
    var ri:Int = 0; while (ri < rowIndices.length) {
      var ci: Int = 0; while (ci < columnIndices.length) {
        vs(i) = apply( rowIndices(ri), columnIndices(ci) )
        i += 1
        ci += 1
      }
      ri += 1
    }
    new MatF[M1, N1](vs)
  }

  /** Get a submatrix.
    *
    * @param r0 Initial row index
    * @param columnIndices  Array of column indices.
    * @return A(i0:i1,c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def subMatrix[M1 <: Int, N1 <: Int](r0: Int, columnIndices: NArray[Int])(using ValueOf[M1], ValueOf[N1]): MatF[M1, N1] = {
    val subRows:Int = valueOf[M1]
    val subColumns:Int = valueOf[N1]
    val rEnd:Int = r0 + subRows

    val vs: NArray[Float] = new NArray[Float](subRows * subColumns)
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
    new MatF[M1, N1](vs)
  }

  /** Get a submatrix.
    *
    * @param r  Array of row indices.
    * @param c0 Initial column index
    * @param c1 Final column index
    * @return A(r(:),j0:j1)
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def subMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], c0: Int)(using ValueOf[M1], ValueOf[N1]): MatF[M1, N1] = {
    val subRows: Int = valueOf[M1]
    val subColumns: Int = valueOf[N1]
    val cEnd = c0 + subColumns
    val vs: NArray[Float] = new NArray[Float](subRows * subColumns)
    var i: Int = 0
    var ri: Int = 0; while (ri < rowIndices.length) {
      var ci: Int = c0; while (ci < cEnd) {
        vs(i) = apply( rowIndices(ri), ci )
        i += 1
        ci += 1
      }
      ri += 1
    }
    new MatF[M1, N1](vs)
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
  def setMatrix[M1 <: Int, N1 <: Int](r0: Int, c0: Int, thatMatrix: MatF[M1, N1])(using ValueOf[M1], ValueOf[N1]): Unit = {
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
  def setMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], columnIndices: NArray[Int], thatMatrix: MatF[M1, N1]): Unit = {
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
  def setMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], c0: Int, thatMatrix: MatF[M1, N1])(using ValueOf[M1], ValueOf[N1]): Unit = {
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
  def setMatrix[M1 <: Int, N1 <: Int](r0: Int, columnIndices: NArray[Int], thatMatrix: MatF[M1, N1])(using ValueOf[M1], ValueOf[N1]): Unit = {
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

  /** MatF transpose.
    *
    * @return Mᵀ
    */
  def transpose: MatF[N, M] = new MatF[N, M](columnPackedArray)

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

  inline def + (B: MatF[M, N]): MatF[M, N] = copy.add(B)

  inline def += (B: MatF[M, N]): MatF[M, N] = add(B)

  /** A = A + B
    *
    * @param B another matrix
    * @return A + B
    */
  def add(B: MatF[M, N]): MatF[M, N] = {
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
  inline def unary_- : MatF[M, N] = * ( -1.0f )

  inline def - (B: MatF[M, N]): MatF[M, N] = copy.subtract(B)

  inline def -= (B: MatF[M, N]): MatF[M, N] = subtract(B)

  /** A = A - B
    *
    * @param B another matrix
    * @return A - B
    */
  def subtract(B: MatF[M, N]): MatF[M, N] = {
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
  def addScalar(d: Float)(using ValueOf[N]): MatF[M,N] = {
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
  inline def * (s: Float): MatF[M, N] = copy.times(s)

  inline def *= (s:Float):MatF[M, N] = times(s)

  /** Add a scalar to a matrix, C = A + s
    *
    * @param s scalar
    * @return A + s
    */
  inline def + (s: Float): MatF[M, N] = copy.addScalar(s)

  inline def += (s:Float):MatF[M, N] = addScalar(s)

  inline def - (s: Float): MatF[M, N] = copy.addScalar(-s)

  inline def -= (s: Float): MatF[M, N] = addScalar(-s)

  /** Multiply a matrix by a scalar in place, A = s*A
    *
    * @param s scalar
    * @return replace A by s*A
    */
  def times(s: Float): MatF[M, N] = {
    var i:Int = 0
    while (i < values.length) {
      values(i) = values(i) * s
      i += 1
    }
    this
  }

  inline def * (v: VecF[N])(using ValueOf[N]): VecF[N] = times(v)

  def times (v: VecF[N])(using ValueOf[N]): VecF[N] = {
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
    a.asInstanceOf[VecF[N]]
  }

  def * [V <: Int](thatMatrix: MatF[N, V])(using ValueOf[V]): MatF[M, V] = times(thatMatrix)

  /** Linear algebraic matrix multiplication, A * B
    *
    * @param b another matrix
    * @return MatF product, A * B
    * @throws IllegalArgumentException MatF inner dimensions must agree.
    */
  def times[V <: Int](b: MatF[N, V])(using ValueOf[V]): MatF[M, V] = {

    val X:MatF[M, V] = MatF.zeros[M, V]

    val Bcolj = new NArray[Float](columns)

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
        X(i, j) = s.toFloat
        i = i + 1
      }
      j = j + 1
    }
    X
  }

  /** MatF trace.
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


  inline def concatenateRows[M1 <: Int](m: MatF[M1, N])(using ValueOf[M1], ValueOf[+[M, M1]]): MatF[+[M, M1], N] = {
    val out:NArray[Float] = new NArray[Float](values.length + m.values.length)
    narr.native.NArray.copyFloatArray( values, 0, out, 0, values.length )
    narr.native.NArray.copyFloatArray( m.values, 0, out, values.length, m.values.length)
    new MatF[+[M, M1], N](out)
  }

  inline def concatenateRows(m: MatF[? <: Int, ? <: Int]): NArray[Float] = {
    val out: NArray[Float] = new NArray[Float](values.length + m.values.length)
    narr.native.NArray.copyFloatArray(values, 0, out, 0, values.length)
    narr.native.NArray.copyFloatArray(m.values, 0, out, values.length, m.values.length)
    out
  }

  inline def concatenateColumns[N1 <: Int](m: MatF[M,N1])(using ValueOf[N1], ValueOf[+[N,N1]]): MatF[M, +[N,N1]] = {
    val out:NArray[Float] = new NArray[Float](values.length + m.values.length)
    var r:Int = 0
    var i:Int = 0
    while (r < rows) {
      narr.native.NArray.copyFloatArray( values, r * columns, out, i, columns )
      i = i + columns
      narr.native.NArray.copyFloatArray( m.values, r * m.columns, out, i, m.columns )
      i = i + m.columns
      r = r + 1
    }
    new MatF[M, +[N, N1]](out)
  }

  inline def concatenateColumns(m: MatF[? <: Int, ? <: Int]): NArray[Float] = {
    val out:NArray[Float] = new NArray[Float](values.length + m.values.length)
    var r:Int = 0
    var i:Int = 0
    while (r < rows) {
      narr.native.NArray.copyFloatArray( values, r * columns, out, i, columns )
      i = i + columns
      narr.native.NArray.copyFloatArray( m.values, r * m.columns, out, i, m.columns )
      i = i + m.columns
      r = r + 1
    }
    out
  }

  def asNativeArray2D: NArray[NArray[Float]] = rowVectors.asInstanceOf[NArray[NArray[Float]]]

  def strictEquals(obj: Any): Boolean = {
    obj match {
      case that: MatF[?, ?] =>
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

  import slash.matrix.MatFormat
  import slash.matrix.Mat

  def render(
    format: MatFormat = MatFormat.DEFAULT,
    alignment: Function2[Mat[? <: Int, ? <: Int], MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): StringBuilder = {
    format.render(this.toMat, alignment, sb)
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

  def upperTriangular: MatF[M,N] = {
    val out:MatF[M, N] = copy
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

  def lowerTriangular: MatF[M,N] = {
    val out:MatF[M, N] = copy
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

  def diagonalVector(using ValueOf[Min[M,N]]): VecF[Min[M,N]] = {
    val dim: Int = valueOf[M].min(valueOf[N])
    val arr: VecF[Min[M,N]] = new FloatArray(dim).asInstanceOf[VecF[Min[M,N]]]
    var i = 0
    while(i < dim) {
      arr(i) = apply(i,i)
      i += 1
    }
    arr
  }

}