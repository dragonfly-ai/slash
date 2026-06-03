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
import slash.exceptions.CannotLinearizeMatrixData

object MatrixData {

  def apply(values:NArray[NArray[Double]]):MatrixData = {
    if (util.linearizeable(values.length, values(0).length)) {
      new LinearizedMatrixData(values.length, values(0).length, util.flatten(values))
    } else new MatrixDataGrid(values)
  }

  /**
   * Create a suitable MatrixData instance given matrix dimensions.
   *
   * @param rowDimension the row dimension.
   * @param columnDimension the column dimension.
   * @return A LinearizedMatrixData or MatrixDataGrid instance.
   */
  def apply(rowDimension:Int, columnDimension:Int):MatrixData = {
    if (util.linearizeable(rowDimension, columnDimension)) LinearizedMatrixData(rowDimension, columnDimension)
    else MatrixDataGrid(rowDimension, columnDimension)
  }

  
  /**
   * Generate MatrixData instance from lambda.
   *
   * @param rowDimension the row dimension.
   * @param columnDimension the column dimension.
   * @param f a function of (row, column) => Double
   * @return a MatrixData instance.
   */
  def tabulate(rowDimension:Int, columnDimension:Int, f:(Int, Int) => Double):MatrixData = {
    val out:MatrixData = apply(rowDimension, columnDimension)
    var r:Int = 0
    while (r < out.rowDimension){
      var c: Int = 0
      while (c < out.columnDimension) {
        out(r, c) = f(r, c)
        c += 1
      }
      r += 1
    }
    out
  }
//
//  def concatenateRows(md1: MatrixData, md2: MatrixData): MatrixData = {
//    val outRowDimension: Int = md1.rowDimension + md2.rowDimension
//
//    if (util.linearizeable(md1.rowDimension + md2.rowDimension, md1.columnDimension)) {
//      md1 match {
//        case lmd1: LinearizedMatrixData =>
//          md2 match {
//            case lmd2:LinearizedMatrixData =>
//              new LinearizedMatrixData(
//                outRowDimension,
//                md1.columnDimension,
//                util.concatenateRows(lmd1.values, lmd2.values)
//              )
//            case mdg:MatrixDataGrid =>
//              val out: NArray[Double] = new NArray[Double](outRowDimension * md1.columnDimension)
//              narr.native.NArray.copyDoubleArray(lmd1.values, 0, out, 0, lmd1.values.length)
//              var r:Int = 0
//              while (r < mdg.rowDimension) {
//                narr.native.NArray.copyDoubleArray(mdg.getRow(r), 0, out, r * md1.rowDimension, md1.columnDimension)
//                r += 1
//              }
//              new LinearizedMatrixData(outRowDimension, md1.columnDimension, out)
//          }
//        case mdg1:MatrixDataGrid =>
//          md2 match {
//            case lmd2: LinearizedMatrixData =>
//              val out: NArray[Double] = new NArray[Double](outRowDimension * md1.columnDimension)
//              var r: Int = 0
//              while (r < mdg1.rowDimension) {
//                narr.native.NArray.copyDoubleArray(mdg1.getRow(r), 0, out, r * mdg1.rowDimension, mdg1.columnDimension)
//                r += 1
//              }
//              narr.native.NArray.copyDoubleArray(lmd2.values, 0, out, mdg1.rowDimension * mdg1.columnDimension, lmd2.values.length)
//              new LinearizedMatrixData(outRowDimension, md1.columnDimension, out)
//            case mdg2: MatrixDataGrid =>
//              val out: NArray[Double] = new NArray[Double](outRowDimension * md1.columnDimension)
//              var r: Int = 0
//              while (r < mdg1.rowDimension) {
//                narr.native.NArray.copyDoubleArray(mdg1.getRow(r), 0, out, r * mdg1.rowDimension, mdg1.columnDimension)
//                r += 1
//              }
//              val offset: Int = mdg1.rowDimension * mdg1.columnDimension
//              r = 0
//              while (r < mdg2.rowDimension) {
//                narr.native.NArray.copyDoubleArray(mdg2.getRow(r), 0, out, offset + r * mdg1.rowDimension, mdg1.columnDimension)
//                r += 1
//              }
//              new LinearizedMatrixData(outRowDimension, md1.columnDimension, out)
//          }
//      }
//    } else {
//      val out:NArray[NArray[Double]] = new NArray[NArray[Double]](outRowDimension)
//
//      val rOffset: Int = md1.rowDimension
//      var r:Int = 0
//      while (r < rOffset) {
//        out(r) = md1.getRow(r)
//        r += 1
//      }
//
//      r = 0
//      while (r < md2.rowDimension ) {
//        out(r + rOffset) = md2.getRow(r)
//        r += 1
//      }
//      new MatrixDataGrid(out)
//    }
//  }

  /**
   * Concatenate matrix rows to form a new matrix.
   *
   * @param md1 a matrix.
   * @param md2 another matrix.
   * @return a matrix with the rows of md2 concatenated to the rows of md1.
   */
  def concatenateRows(md1: MatrixData, md2: MatrixData): MatrixData = {
    val outRowDimension: Int = md1.rowDimension + md2.rowDimension

    val out:NArray[NArray[Double]] = new NArray[NArray[Double]](outRowDimension)

    val rOffset: Int = md1.rowDimension
    var r:Int = 0
    while (r < rOffset) {
      out(r) = md1.getRow(r)
      r += 1
    }

    r = 0
    while (r < md2.rowDimension ) {
      out(r + rOffset) = md2.getRow(r)
      r += 1
    }

    if (util.linearizeable(outRowDimension, md1.columnDimension)) {
      new LinearizedMatrixData(outRowDimension, md1.columnDimension, util.flatten(out))
    } else new MatrixDataGrid(out)

  }

  /**
   * Concatenate the columns of two matrices to form a new matrix.
   *
   * @param md1 a matrix.
   * @param md2 another matrix.
   * @return a matrix with the columns of md2 concatenated to the columns of md1.
   */

  def concatenateColumns(md1: MatrixData, md2: MatrixData) = {
    val outColDimension: Int = md1.columnDimension + md2.columnDimension
    val rows = md1.rowDimension
    val out: NArray[NArray[Double]] = new NArray[NArray[Double]](rows)

    var r:Int = 0
    while (r < rows) {
      out(r) = util.concatenateRows(md1.getRow(r), md2.getRow(r))
      r += 1
    }

    if (util.linearizeable(md1.rowDimension, outColDimension)) {
      new LinearizedMatrixData(md1.rowDimension, outColDimension, util.flatten(out))
    } else new MatrixDataGrid(out)
  }

}

trait MatrixData {

  /**
   * @return The dimension of the matrix rows.
   */
  def rowDimension:Int

  /**
   * @return The dimension of the matrix columns.
   */
  def columnDimension:Int

  /** Get a single element.
   *
   * @param r Row index.
   * @param c Column index.
   * @return A(i,j)
   * @throws ArrayIndexOutOfBoundsException
   */
  def apply(r: Int, c: Int): Double

  /** Set a single element.
   *
   * @param r Row index.
   * @param c Column index.
   * @param value values(i,j).
   * @throws ArrayIndexOutOfBoundsException
   */
  def update(r: Int, c: Int, value: Double): Unit

  /** Copy this instance of MatrixData
   *
   * @return A deep copy of this MatrixData instance.
   */
  def copy:MatrixData

  /**
   * Can the underlying MatrixData fit into a 1D NArray[Double]?
   * @return true if the elements of this array can fit into a single 1D NArray[Double], otherwise false.
   */
  def linearizeable:Boolean

  /** Make a one-dimensional column packed copy of the internal array.
   *
   * @return MatrixData elements packed in a one-dimensional array by columns.
   */
  def columnPackedNArray: NArray[Double]


  /** Make a one-dimensional row packed copy of the internal array.
   *
   * @return MatrixData elements packed in a one-dimensional array by rows.
   */
  def rowPackedNArray: NArray[Double]

  /**
   * MatrixData transpose.
   *
   * @return Mᵀ
   */
  def transpose: MatrixData

  /**
   * extract a copy of a row.
   * @param row the row of the matrix to return as a vector.
   * @return a copy of the specified matrix row in NArray[Double] format.
   */
  def getRow(row:Int):NArray[Double]

  def flatten:NArray[Double]
}

object LinearizedMatrixData {
  def apply(rowDimension:Int, columnDimension:Int):LinearizedMatrixData = {
    new LinearizedMatrixData(rowDimension, columnDimension, new NArray[Double](rowDimension * columnDimension))
  }
}

class LinearizedMatrixData (
  override val rowDimension:Int,
  override val columnDimension:Int,
  val values:NArray[Double]
) extends MatrixData {

  require(
    rowDimension * columnDimension == values.length,
    s"Rows * Columns must == values.length, however: $rowDimension * $columnDimension != ${values.length}"
  )

  /**
   * Compute the linear index of a 2D coordinate of a matrix array literal.
   *
   * @param r       the matrix row.
   * @param c       the matrix column.
   * @param columns number of columns in the matrix.
   * @return
   */
  private inline def lindex(r: Int, c: Int, columns: Int): Int = (r * columns) + c

  /** Get a single element.
   *
   * @param r Row index.
   * @param c Column index.
   * @return A(i,j)
   * @throws ArrayIndexOutOfBoundsException
   */
  override inline def apply(r: Int, c: Int): Double = values(lindex(r, c, columnDimension))

  /** Set a single element.
   *
   * @param r     Row index.
   * @param c     Column index.
   * @param value values(i,j).
   * @throws ArrayIndexOutOfBoundsException
   */
  override inline def update(r: Int, c: Int, value: Double): Unit = values(lindex(r, c, columnDimension)) = value

  /** Copy this instance of LinearizedMatrixData
   *
   * @return A deep copy of this LinearizedMatrixData instance.
   */
  inline def copy: LinearizedMatrixData = new LinearizedMatrixData(rowDimension, columnDimension, narr.copy[Double](values))

  /**
   * Can the underlying MatrixData fit into a 1D NArray[Double]?
   *
   * @return true if the elements of this array can fit into a single 1D NArray[Double], otherwise false.
   */
  def linearizeable: Boolean = true

  /** Make a one-dimensional column packed copy of the internal array.
   *
   * @return MatrixData elements packed in a one-dimensional array by columns.
   */
  def columnPackedNArray: NArray[Double] = {
    val vs: NArray[Double] = new NArray[Double](values.length)
    var i: Int = 0
    while (i < rowDimension) {
      var j: Int = 0
      while (j < columnDimension) {
        vs(i + j * rowDimension) = values(lindex(i, j, columnDimension))
        j = j + 1
      }
      i = i + 1
    }
    vs
  }

  /** Make a one-dimensional row packed copy of the internal array.
   *
   * @return LinearizedMatrixData elements packed in a one-dimensional array by rows.
   */
  inline def rowPackedNArray: NArray[Double] = NArray.copy[Double](values)

  /**
   * Mat transpose.
   *
   * @return Mᵀ
   */
  override def transpose: MatrixData = new LinearizedMatrixData(columnDimension, rowDimension, columnPackedNArray)

  /**
   * extract a copy of a row.
   *
   * @param row the row of the matrix to return as a vector.
   * @return a copy of the specified matrix row in NArray[Double] format.
   */
  override def getRow(row:Int): NArray[Double] = {
    values.asInstanceOf[NArr[Double]].slice(row * columnDimension, (row * columnDimension) + columnDimension).asInstanceOf[NArray[Double]]
  }

  /**
   * Get a flattened NArray of the values from this MatrixData.
   *
   * @return a flattened NArray of the values from this MatrixData.
   */
  def flatten:NArray[Double] = NArray.copy[Double](values)

}

object MatrixDataGrid {
  def apply(rowDimension:Int, columnDimension:Int): MatrixDataGrid = new MatrixDataGrid(
    NArray.tabulate[NArray[Double]](rowDimension)(_ => new NArray[Double](columnDimension))
  )
}

class MatrixDataGrid (
  val values:NArray[NArray[Double]]
) extends MatrixData {

  /**
   * @return The dimension of the matrix rows.
   */
  override val rowDimension: Int = values.length

  /**
   * @return The dimension of the matrix columns.
   */
  override val columnDimension: Int = values(0).length


  /** Get a single element.
   *
   * @param r Row index.
   * @param c Column index.
   * @return A(i,j)
   * @throws ArrayIndexOutOfBoundsException
   */
  override inline def apply(r: Int, c: Int): Double = values(r)(c)

  /** Set a single element.
   *
   * @param r     Row index.
   * @param c     Column index.
   * @param value values(i,j).
   * @throws ArrayIndexOutOfBoundsException
   */
  override inline def update(r: Int, c: Int, value: Double): Unit = values(r)(c) = value

  /** Copy this instance of MatrixDataGrid
   *
   * @return A deep copy of this MatrixDataGrid instance.
   */
  inline def copy: MatrixDataGrid = new MatrixDataGrid(util.copy(values))

  /**
   * Can the underlying MatrixData fit into a 1D NArray[Double]?
   *
   * @return true if the elements of this array can fit into a single 1D NArray[Double], otherwise false.
   */
  override def linearizeable:Boolean = util.linearizeable(rowDimension, columnDimension)

  /** Make a one-dimensional column packed copy of the internal array.
   *
   * @return MatrixDataGrid elements packed in a one-dimensional array by columns.
   */
  def columnPackedNArray: NArray[Double] = {
    if (linearizeable) {
      var i:Int = 0
      val colPacked = new NArray[Double](rowDimension * columnDimension)

      var r: Int = 0
      while (r < rowDimension) {
        var c: Int = 0
        while (c < columnDimension) {
          colPacked(i) = values(r)(c)
          i = i + 1
          c = c + 1
        }
        r = r + 1
      }

      colPacked
    } else throw CannotLinearizeMatrixData(rowDimension, columnDimension)
  }

  /** Make a one-dimensional row packed copy of the internal array.
   *
   * @return LinearizedMatrixData elements packed in a one-dimensional array by rows.
   */
  inline def rowPackedNArray: NArray[Double] = {
    if (linearizeable) util.flatten(values)
    else throw CannotLinearizeMatrixData(rowDimension, columnDimension)
  }

  /**
   * Mat transpose.
   *
   * @return Mᵀ
   */
  override def transpose: MatrixDataGrid = new MatrixDataGrid({
    val transposed = new NArray[NArray[Double]](columnDimension)

    var c: Int = 0
    while (c < columnDimension) {
      transposed(c) = new NArray[Double](rowDimension)
      var r: Int = 0
      while (r < rowDimension) {
        transposed(c)(r) = values(r)(c)
        r = r + 1
      }
      c = c + 1
    }

    transposed
  })

  /**
   * extract a copy of a row.
   *
   * @param row the row of the matrix to return as a vector.
   * @return a copy of the specified matrix row in NArray[Double] format.
   * @throws ArrayIndexOutOfBoundsException
   */
  override def getRow(row:Int): NArray[Double] = NArray.copy[Double](values(row))

  /**
   * Get a flattened NArray of the values from this MatrixData.
   *
   * @return a flattened NArray of the values from this MatrixData.
   * @throws CannotLinearizeMatrixData
   */
  def flatten: NArray[Double] = {
    if (linearizeable) util.flatten(values)
    else throw CannotLinearizeMatrixData(rowDimension, columnDimension)
  }
}