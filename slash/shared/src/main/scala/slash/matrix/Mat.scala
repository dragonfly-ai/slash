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
import scala.compiletime.{constValue, summonFrom, error}
import scala.math.hypot
import scala.Tuple.*

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


  /** Construct a matrix by copying a one-dimensional packed array
   *
   * @param values One-dimensional array of doubles, packed by rows.
   * @param A    number of rows.
   * @throws IllegalArgumentException Array length must be a multiple of A.
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

  /** Construct a Mat from a tuple literal.
   *  `((a,b,c...),(d,e,f,...),...)`. << nested tuple fields define rows
   *  `(a,b,c...)`.                   << fields of single tuple define columns
   *
   *    if nested tuples share the same arity
   *    tuple Numeric fields converted to type Double
   *    non-numeric fields, if present converted to `Double.NaN`
   *
   * @throws IllegalArgumentException on non-numeric fields.
   * @param tuparg zero or more Tuples each with M numeric columns
   * @return an M x N matrix consisting of values.
   */
  transparent inline def apply[M <: Int, N <: Int](tuparg: Tuple)(using ValueOf[M], ValueOf[N]): Mat[M,N] = {
    val rows: Int = valueOf[M]
    val cols: Int = valueOf[N]
    val matsize1: Int = rows * cols
    val values:NArray[Double] = new NArray[Double](matsize1)
    var i:Int = 0
    var j:Int = 0
    def iterateRow(iter: Iterator[Any]): Unit = {
      iter.foreach {
        case nested: Product if nested.productArity > 0 => // Check if it's a nested tuple or product
          j = 0
          iterateRow(nested.productIterator)
          require(j == cols, s"# j[$j] != cols[$cols]")
        case element: Number =>
          values(i) = toDouble(element) // Process non-tuple elements
          i += 1
          j += 1
        case x =>
          throw new IllegalArgumentException(s"non-numeric field [$x] [${x.getClass}]")
      }
    }
    iterateRow(tuparg.productIterator)
    new Mat[M,N](values)
  }


  /** Construct a Mat from a sequence of 2 or more Tuple literals.
   *  `((a,b,c...),(d,e,f,...),...)`.
   *
   * Where:
   *    inner row tuples share the same arity
   *    tuple Numeric fields converted to type Double
   *    non-numeric fields, if present converted to `Double.NaN`
   * @param tup a tuple with M Number tuples of arity N
   * @return an M x N matrix consisting of values.
   */
  transparent inline def fromTuples[M <: Int, N <: Int](tuprows: Tuple *)(using ValueOf[M], ValueOf[N]): Mat[M, N] = {
    val rows: Int = valueOf[M]
    val cols: Int = valueOf[N]
    val matsize1: Int = rows * cols
    val values:NArray[Double] = new NArray[Double](matsize1)
    var i:Int = 0
    var j:Int = 0
    def iterateRow(iter: Iterator[Any]): Unit = {
      iter.foreach {
        case nested: Product if nested.productArity > 0 => // Check if it's a nested tuple or product
          j = 0
          iterateRow(nested.productIterator)
          require(j == cols, s"j[$j] != cols[$cols]")
        case element: Number =>
          values(i) = toDouble(element) // Process non-tuple elements
          i += 1
          j += 1
        case x =>
          throw new IllegalArgumentException(s"non-numeric field [$x] [${x.getClass}]")
      }
    }
    iterateRow(tuprows.iterator)
    new Mat[M,N](values)
  }


  /** Construct a Mat from a String.
   * @param content a String with rows of delimited numeric columns.
   * @tparam M the number of rows
   * @tparam N the number of columns
   * @return an M x N matrix
   */
  inline def apply[M <: Int, N <: Int](inline content: String)(using ValueOf[M], ValueOf[N]): Mat[M, N] = {
    val matrix = Util.fromString(content)
    val (rows, cols) = (matrix.rows, matrix.columns)
    val (r, c) = (valueOf[M], valueOf[N])
    require(
      r == rows && c == cols,
      s"expecting $r x $c but found $rows x $cols"
    )
    matrix.cast[M, N]
  }

  /** Horizontal tiling of one or more matrices (having same number of rows) */
  inline def horzcat[M <: Int](matrices: Mat[M,?] *)(using ValueOf[M]): Mat[M,? <: Int] = {
    if (matrices.isEmpty) throw new IllegalArgumentException("empty parameter list")
    val numRows = matrices.map(_.rows).toList.distinct match {
    case num :: Nil =>
      num
    case list =>
      sys.error(s"Matrices have diverse number of rows: $list")
    }
    val numCols = matrices.map(_.columns).sum
    val res = Mat.zeros[numRows.type,numCols.type]
    var offset = 0
    for (m <- matrices) {
      for (i <- 0 until numRows) {
        for (j <- 0 until m.columns) {
          res(i,j+offset) = m(i,j)
        }
      }
      offset += m.columns
    }
    res.asInstanceOf[Mat[M,numCols.type]]
  }

  /** Vertical tiling of one or more matrices (having same number of columns)
   */
  inline def vertcat[N <: Int](matrices: Mat[?,N] *)(using ValueOf[N]): Mat[? <: Int, N] = {
    if (matrices.isEmpty) throw new IllegalArgumentException("empty parameter list")
    val numCols = matrices.map(_.columns).toList.distinct match {
    case num :: Nil =>
      num
    case list =>
      sys.error(s"Matrices have diverse number of columns: $list")
    }
    val numRows = matrices.map(_.rows).sum
    val res = Mat.zeros[numRows.type, numCols.type]
    var offset = 0
    for (m <- matrices) {
      for (i <- 0 until m.rows) {
        for (j <- 0 until numCols) {
          res(i+offset,j) = m(i,j)
        }
      }
      offset += m.rows
    }
    res.asInstanceOf[Mat[numRows.type, N]]
  }

  /**
   * Support left add / multiply by scalars.
   */
  extension(s: Double) {
    inline def +[M <: Int, N <: Int](inline m: Mat[M,N])(using ValueOf[M], ValueOf[N]): Mat[M,N] = m.copy.addScalar(s)
    inline def +=[M <: Int, N <: Int](inline m: Mat[M,N])(using ValueOf[M], ValueOf[N]): Mat[M,N] = m.addScalar(s)
    inline def *[M <: Int, N <: Int](inline m: Mat[M,N])(using ValueOf[M], ValueOf[N]): Mat[M,N] = m.copy.times(s)
    inline def *=[M <: Int, N <: Int](inline m: Mat[M,N])(using ValueOf[M], ValueOf[N]): Mat[M,N] = m.times(s)
  }

  extension(s: Int) {
    inline def +[M <: Int, N <: Int](inline m: Mat[M,N])(using ValueOf[M], ValueOf[N]): Mat[M,N] = m.copy.addScalar(s.toDouble)
    inline def +=[M <: Int, N <: Int](inline m: Mat[M,N])(using ValueOf[M], ValueOf[N]): Mat[M,N] = m.addScalar(s.toDouble)
    inline def *[M <: Int, N <: Int](inline m: Mat[M,N])(using ValueOf[M], ValueOf[N]): Mat[M,N] = m.copy.times(s.toDouble)
    inline def *=[M <: Int, N <: Int](inline m: Mat[M,N])(using ValueOf[M], ValueOf[N]): Mat[M,N] = m.times(s.toDouble)
  }

  type Number = Int | Float | Long | Double

  type IsSingleton[T] <: Boolean = T match
    case Singleton => true // Matches singleton types
    case _ => false        // Matches all other types

  // recursive compile-time type functions
  // S is the Int successor type, denoting N + 1 at the type level
  type RowSize[T <: Tuple] <: Int = T match
    case EmptyTuple => 0
    case h *: t => S[RowSize[t]]

  type ColSize[T <: Tuple] <: Int = T match
    case EmptyTuple => 0
    case h *: _ => TupleSize[h]

  type TupleSize[T <: Tuple] <: Int = T match
    case EmptyTuple => 0
    case h *: t => S[TupleSize[t]]

  type Prod[A <: Int, B <: Int] <: Int = A match
    case 0 => 0
    case S[aMinus1] => B + Prod[aMinus1, B]

  type TupleMatsize[T <: Tuple] = Prod[RowSize[T], ColSize[T]]

  inline def toDouble(inline num: Number): Double = {
    num match {
    case d: Double     => d
    case d: Int        => d.toDouble
    case d: Long       => d.toDouble
    case d: Float      => d.toDouble
    }
  }

  inline def dims[T <: Tuple](inline tup: T): (Int, Int) = {
    (constValue[RowSize[T]], constValue[ColSize[T]])
  }

  inline def sameSpace[M <: Int, N <: Int, P <: Int, Q <: Int](mat1: Mat[M, N], mat2: Mat[P, Q]): Boolean = {
    summonFrom {
      case ev1: (M =:= P) =>
        summonFrom {
          case ev2: (N =:= Q) => true
          case _ => false
        }
      case _ => false
    }
  }

}

class Mat[M <: Int, N <: Int](val values: NArray[Double])(using ValueOf[M], ValueOf[N]) {

  val rows: Int = valueOf[M]
  val columns: Int = valueOf[N]
  def dims = (rows,columns)

  val MxN: Int = rows * columns
  opaque type MN <: Int = M * N

  require(rows * columns == values.length, s"Product of $rows x $columns != ${values.length}")

  inline def lindex(inline r:Int, c:Int):Int = {
    (rowIndex(r) * columns) + colIndex(c)
  }
  // negative index is relative to max index
  inline def rowIndex(inline r:Int):Int = if r < 0 then r + rows else r
  inline def colIndex(inline c:Int):Int = if c < 0 then c + columns else c

  /** @return a row-major Vec */
  def flatten = rowPackedArray.asInstanceOf[Vec[MN]]

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

  /** A = A + d
    *
    * @param d a Double
    * @return A element-wise plus d
    */
  def + (d: Double): Mat[M, N] = copy.addScalar(d)
  def += (d: Double): Mat[M, N] = addScalar(d)
  
  /** A = A + d
    *
    * @param d an Int
    * @return A element-wise plus d
    */
  def + (d: Int): Mat[M, N] = copy.addScalar(d.toDouble)
  def += (d: Int): Mat[M, N] = addScalar(d.toDouble)

  /** A = A + d
  *
  * @param d a scalar
  * @return A + d
  */
  def addScalar(d: Double)(using ValueOf[N]): Mat[M,N] = {
    for(i <- 0 until rows){
      for(j <- 0 until columns){
        apply(i,j) += d
      }
    }
    this
  }

  /** A = A + d
  *
  * @param d a scalar
  * @return A + d
  */
  def addScalar(d: Int)(using ValueOf[N]): Mat[M,N] = {
    for(i <- 0 until rows){
      for(j <- 0 until columns){
        apply(i,j) += d.toDouble
      }
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
  inline def * (s: Double): Mat[M, N] = copy.times(s)

  inline def *= (s:Double):Mat[M, N] = times(s)

  /** Multiply a matrix by a scalar in place, A = s*A
    *
    * @param s scalar
    * @return replace A by s*A
    */
  inline def times(s: Double): Mat[M, N] = {
    var i:Int = 0; while (i < values.length) {
      values(i) = values(i) * s
      i += 1
    }
    this
  }
  inline def times(num: Int): Mat[M, N] = {
    inline val s: Double = num.toDouble
    var i:Int = 0; while (i < values.length) {
      values(i) = values(i) * s
      i += 1
    }
    this
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

  def asNativeArray2D:NArray[NArray[Double]] = rowVectors.asInstanceOf[NArray[NArray[Double]]]

  override def toString: String = {
    val sb: StringBuilder = StringBuilder()
    var r: Int = 0
    while (r < rows) {
      sb.append("\n")
      var c: Int = 0
      while (c < columns) {
        sb.append(s"${apply(r, c)}, ")
        c = c + 1
      }
      r = r + 1
    }
    sb.toString()
  }

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

  /** 
   *  Matrix row vector view.
   */
  inline def apply(inline row:Int, cons: ::.type): MatRow = {
    new MatRow(row)
  }

  /** 
   *  Matrix column vector view.
   */
  inline def apply(cons: ::.type, inline column:Int): MatCol = {
    new MatCol(column)
  }

  /**
   * Wrapper class to represent row on LHS or RHS
   */
  class MatRow(r: Int) {
    val row = rowIndex(r)
    inline def :=(inline vector: Vec[N]): Unit = {
      val start: Int = row * columns
      var i = 0
      while(i < columns) {
        values(start+i) = vector(i)
        i += 1
      }
    }
    def show: String = rowVector(row).show
    def asVec: Vec[N] = rowVector(row)
  }

  /**
   * Wrapper class to represent column on LHS or RHS
   */
  class MatCol(c: Int) {
    val column = colIndex(c)
    inline def :=(inline vector: Vec[M]): Unit = {
      var row = 0
      while(row < rows) {
        values(row * columns + column) = vector(row)
        row += 1
      }
    }
    def show: String = columnVector(column).show
    def asVec: Vec[M] = columnVector(column)
  }

  import scala.language.implicitConversions
  given matRowConversion: Conversion[MatRow, Vec[N]] with
    def apply(rowSlice: MatRow): Vec[N] = rowSlice.asVec

  given matColConversion: Conversion[MatCol, Vec[M]] with
    def apply(colSlice: MatCol): Vec[M] = colSlice.asVec

  export this.given
}

