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
import slash.vector.*

import scala.compiletime.ops.int.*
import scala.math.hypot
import narr.NArray
import scala.compiletime.{constValue, summonInline}

/**
  * This library is fundamentally an adaptation of the Java Matrix library, JaMa, by MathWorks Inc. and the National Institute of Standards and Technology.
  */

object Matrix {
  /** Construct a matrix from a copy of an array.
   *
   * @param values array of doubles.
   * @throws IllegalArgumentException All rows must have the same length
   */
  inline def copyFrom[M <: Int, N <: Int](values: NArray[Double])(using ValueOf[M], ValueOf[N]): Matrix[M, N] = apply(narr.copy[Double](values))

  inline def random[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]): Matrix[M, N] = random[M, N](slash.Random.defaultRandom)

  /**
   * Generates an MxN matrix which consists of elements randomized between [-1.0, 1.0] inclusive.
   * @param r optional random instance.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return An MxN matrix with uniformly distributed random elements.
   */
  inline def random[M <: Int, N <: Int](r:scala.util.Random)(using ValueOf[M], ValueOf[N]):Matrix[M, N] = {
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
  )(using ValueOf[M], ValueOf[N]): Matrix[M, N] = random[M, N](minNorm, normMAX, slash.Random.defaultRandom)


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
  )(using ValueOf[M], ValueOf[N]): Matrix[M, N] = random[M, N](slash.interval.`[]`(minNorm, normMAX), r)

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
  )(using ValueOf[M], ValueOf[N]): Matrix[M, N] = new Matrix[M, N](
    NArray.tabulate[Double]( valueOf[M] * valueOf[N] )( _ => interval.random(r) )
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
   * @param arr2d Two-dimensional array of doubles.  arr2d(row)(column).
   * @throws IllegalArgumentException All rows must have the same length
   */

  def apply[M <: Int, N <: Int](arr2d:NArray[Vec[N]])(using ValueOf[M], ValueOf[N]):Matrix[M, N] = {
    val columns:Int = valueOf[N]
    var r:Int = 0; while (r < arr2d.length) {
      if (arr2d(r).dimension != columns) throw new IllegalArgumentException("Cannot create a Matrix from a Jagged Array.")
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
    new Matrix[M, N](values)
  }


  /** Construct an MxN constant matrix.
   * @param value Fill the matrix with this scalar value.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return an MxN constant matrix.
   */
  def fill[M <: Int, N <: Int](value: Double)(using ValueOf[M], ValueOf[N]):Matrix[M, N] = apply[M, N](
    NArray.fill[Double](valueOf[M] * valueOf[N])(value)
  )

  /** Construct an MxN matrix of zeros.
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   */
  def zeros[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]):Matrix[M, N] = fill[M, N](0.0)

  /** Construct an MxN matrix of ones.
   *
   * @tparam M the number of rows
   * @tparam N the number of columns
   */
  def ones[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]): Matrix[M, N] = fill[M, N](1.0)


  /** Construct a matrix by copying a one-dimensional packed array
   *
   * @param values One-dimensional array of doubles, packed by rows.
   * @param A    Number of rows.
   * @throws IllegalArgumentException Array length must be a multiple of A.
   */
  def apply[M <: Int, N <: Int](values: NArray[Double])(using ValueOf[M], ValueOf[N]):Matrix[M, N] = new Matrix[M, N](values)

  /**
   *
   * @param values the matrix elements.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return an M x N matrix consisting of values.
   */
  def apply[M <: Int, N <: Int](values: Double*)(using ValueOf[M], ValueOf[N]):Matrix[M, N] = {
    dimensionCheck(values.size, valueOf[M] * valueOf[N])
    new Matrix[M, N](NArray[Double](values: _*))
  }

  /** Construct a Matrix from Tuple literal, with optional dimensions at call site.
   *  `((a,b,c...),(d,e,f,...),...)`.
   *
   * Where:
   *    inner tuples share the same arity
   *    tuple Numeric fields converted to type Double
   *    non-numeric fields, if present converted to `Double.NaN`
   * @param tup a tuple with M Number tuples of arity N
   * @return an M x N matrix consisting of values.
   */
  transparent inline def Mat[T <: Tuple & Singleton](inline t: T)(using ValueOf[RowSize[T]], ValueOf[ColSize[T]], ValueOf[MatrixSize[T]]) = {
    //dimensionCheck(t.productArity, dimension)
    val rows = valueOf[RowSize[T]]
    val cols = valueOf[ColSize[T]]
    val itr1:Iterator[Any] = t.productIterator
    val matsize1: Int = valueOf[MatrixSize[T]]
    //val matsize2: Int = summon[ValueOf[MatrixSize[T]]].value
    val v:NArray[Double] = new NArray[Double](matsize1)
    var i:Int = 0
    while (itr1.hasNext) {
      val inner = itr1.next().asInstanceOf[Tuple]
      val itr2:Iterator[Any] = inner.productIterator
      while (itr2.hasNext) {
        v(i) = toDouble(itr2.next().asInstanceOf[Number])
        i += 1
      }
    }
    inline (rows, cols) match {
    case (r, c) =>
      new Matrix[r.type, c.type](v)
    }
  }

  type Number = Double | Int | Long | Float | BigDecimal

  // recursive compile-time type functions
  type RowSize[T <: Tuple] <: Int = T match
    case EmptyTuple => 0
    case h *: t => S[RowSize[t]]

  type ColSize[T <: Tuple] <: Int = T match
    case EmptyTuple => 0
    case h *: _ => TupleSize[h]

  type TupleSize[T <: Tuple] <: Int = T match
    case EmptyTuple => 0
    case h *: t => S[TupleSize[t]]

  type Product[A <: Int, B <: Int] <: Int = A match
    case 0 => 0
    case S[aMinus1] => B + Product[aMinus1, B]

  type MatrixSize[T <: Tuple] = Product[RowSize[T], ColSize[T]]


  inline def dims[T <: Tuple](inline tup: T): (Int, Int) = {
    (constValue[RowSize[T]], constValue[ColSize[T]])
  }


  inline def toDouble(inline num: Number): Double = {
    num match {
    case d: Double     => d
    case d: Int        => d.toDouble
    case d: Long       => d.toDouble
    case d: Float      => d.toDouble
    case d: BigDecimal => d.toDouble
    }
  }

  /*
  transparent inline def Mat[T <: Tuple](inline tup: T) = {
    def values: NArray[Double] = tup.productIterator.map {
      case t: Tuple => t.productIterator.map {
        case n: Number => toDouble(n)
      }.toArray.asInstanceOf[NArray[Double]]
    }.toArray.flatten.asInstanceOf[NArray[Double]]
    val (rows: Int, cols: Int) = dims(tup)
    def values: NArray[Double] = tup.productIterator.map {
      case t: Tuple => t.productIterator.map {
        case n: Number => toDouble(n)
      }.toArray.asInstanceOf[NArray[Double]]
    }.toArray.flatten.asInstanceOf[NArray[Double]]
    //val matsize = constValue[MatSize[T]]
    val values: NArray[Double] = new NArray[Double](rows * cols)
    var i = 0
    tup.productIterator.map {
      case t: Tuple =>
        t.productIterator.map {
          case n: Number =>
            values(i) = toDouble(n)
            i += 1
        }
    }
    inline dims(tup) match {
    case (r, c) =>
      new Matrix[r.type, c.type](values)
    }
  }
  */
}

class Matrix[M <: Int, N <: Int] (val values: NArray[Double])(using ValueOf[M], ValueOf[N]) {

  val rows: Int = valueOf[M]
  val columns: Int = valueOf[N]

  val MxN: Int = rows * columns
  opaque type MN <: Int = M * N

  require(rows * columns == values.length, s"Product of $rows x $columns != ${values.length}")

  inline def lindex(r:Int, c:Int):Int = (r * columns) + c

  /** Make a deep copy of a matrix
    */
  def copy: Matrix[M, N] = new Matrix[M, N](copyValues)

  /** Copy the internal two-dimensional array.
    *
    * @return Two-dimensional array copy of matrix elements.
    */
  inline def copyValues: NArray[Double] = narr.copy[Double](values)

  /** Make a one-dimensional column packed copy of the internal array.
    *
    * @return Matrix elements packed in a one-dimensional array by columns.
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
    * @return Matrix elements packed in a one-dimensional array by rows.
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
  def subMatrix[M1 <: Int, N1 <: Int](r0: Int, c0: Int)(using ValueOf[M1], ValueOf[N1]): Matrix[M1, N1] = {

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
    new Matrix[M1, N1](vs)

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
    new Matrix[M1, N1](vs)
  }

  /** Get a submatrix.
    *
    * @param r0 Initial row index
    * @param columnIndices  Array of column indices.
    * @return A(i0:i1,c(:))
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def subMatrix[M1 <: Int, N1 <: Int](r0: Int, columnIndices: NArray[Int])(using ValueOf[M1], ValueOf[N1]): Matrix[M1, N1] = {
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
    new Matrix[M1, N1](vs)
  }

  /** Get a submatrix.
    *
    * @param r  Array of row indices.
    * @param c0 Initial column index
    * @param c1 Final column index
    * @return A(r(:),j0:j1)
    * @throws ArrayIndexOutOfBoundsException Submatrix indices
    */
  def subMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], c0: Int)(using ValueOf[M1], ValueOf[N1]): Matrix[M1, N1] = {
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
    new Matrix[M1, N1](vs)
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
  def setMatrix[M1 <: Int, N1 <: Int](r0: Int, c0: Int, thatMatrix: Matrix[M1, N1])(using ValueOf[M1], ValueOf[N1]): Unit = {
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
  def setMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], columnIndices: NArray[Int], thatMatrix: Matrix[M1, N1]): Unit = {
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
  def setMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], c0: Int, thatMatrix: Matrix[M1, N1])(using ValueOf[M1], ValueOf[N1]): Unit = {
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
  def setMatrix[M1 <: Int, N1 <: Int](r0: Int, columnIndices: NArray[Int], thatMatrix: Matrix[M1, N1])(using ValueOf[M1], ValueOf[N1]): Unit = {
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

  /** Matrix transpose.
    *
    * @return Mᵀ
    */
  def transpose: Matrix[N, M] = new Matrix[N, M](columnPackedArray)

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
        update(r, c, apply(r, c) + B(r, c))
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
        update(r, c, apply(r, c) - B(r, c))
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
    var i:Int = 0; while (i < values.length) {
      values(i) = values(i) * s
      i += 1
    }
    this
  }

  def * [V <: Int](thatMatrix: Matrix[N, V])(using ValueOf[V]): Matrix[M, V] = times(thatMatrix)

  /** Linear algebraic matrix multiplication, A * B
    *
    * @param b another matrix
    * @return Matrix product, A * B
    * @throws IllegalArgumentException Matrix inner dimensions must agree.
    */
  def times[V <: Int](b: Matrix[N, V])(using ValueOf[V]): Matrix[M, V] = {

    val X:Matrix[M, V] = Matrix.zeros[M, V]

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

  /** Matrix trace.
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

  def asNativeArray2D:NArray[NArray[Double]] = rowVectors.asInstanceOf[NArray[NArray[Double]]]

  override def toString: String = {
    val sb: StringBuilder = StringBuilder()
    var r: Int = 0;
    while (r < rows) {
      sb.append("\n")
      var c: Int = 0;
      while (c < columns) {
        sb.append(s"${apply(r, c)}, ")
        c = c + 1
      }
      r = r + 1
    }
    sb.toString()
  }

}
