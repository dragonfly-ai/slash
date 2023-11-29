package verification

import narr.NArray

import scala.Console.*

object Verification {

  def arrayCompare(a1:NArray[Double], a2:NArray[Double]):MatrixComparison = {

    if (a1.length == a2.length) {

      var discrepancies: Int = 0
      var combinedError: Double = 0.0
      var i: Int = 0;
      while (i < a1.length) {

        if (a1(i) != a2(i)) {
          discrepancies += 1
          val error: Double = Math.abs(a1(i) - a2(i))
          combinedError += error
        }

        i += 1
      }

      MatrixComparison(1, a1.length, discrepancies, combinedError)
    } else throw new Exception(s"${Console.RED}Dimensions do not match!${Console.RESET}")

  }

  def arrayCompare2D(a1:NArray[NArray[Double]], a2:NArray[NArray[Double]]):MatrixComparison = {

    if (a1.length == a2.length && a1(0).length == a2(0).length) {

      var discrepancies: Int = 0
      var r: Int = 0;
      var combinedError: Double = 0.0

      while (r < a1.length) {
        var c: Int = 0;
        while (c < a1(0).length) {
          val v0: Double = a1(r)(c)
          val v1: Double = a2(r)(c)

          if (v0 != v1) {
            val error: Double = Math.abs(v0 - v1)
            discrepancies += 1
            combinedError += error
          }
          c += 1
        }
        r += 1
      }

      MatrixComparison(a1.length, a1(0).length, discrepancies, combinedError)

    } else throw new Exception(s"${Console.RED}Dimensions do not match!${Console.RESET} sm[${a1.length}x${a1(0).length}] vs jm[${a2.length}x${a2(0).length}]")

  }

  def matrixCompare[M <: Int, N <: Int](jm: Jama.Matrix, sm: slash.matrix.Matrix[M, N])(using ValueOf[M], ValueOf[N]): MatrixComparison = {
    matrixCompare[M, N](sm, jm)
  }

    def matrixCompare[M <: Int, N <: Int](sm: slash.matrix.Matrix[M, N], jm: Jama.Matrix)(using ValueOf[M], ValueOf[N]): MatrixComparison = {

    if (sm.rows == jm.getRowDimension && sm.columns == jm.getColumnDimension) {

      var discrepancies: Int = 0
      var r: Int = 0;
      var combinedError: Double = 0.0

      while (r < sm.rows) {
        var c: Int = 0;
        while (c < sm.columns) {
          val v0: Double = sm(r,c)
          val v1: Double = jm.get(r,c)

          if (v0 != v1) {
            val error: Double = Math.abs(v0 - v1)
            discrepancies += 1
            combinedError += error
          }
          c += 1
        }
        r += 1
      }

      MatrixComparison(sm.rows, sm.columns, discrepancies, combinedError)

    } else throw new Exception(s"${Console.RED}Dimensions do not match!${Console.RESET} sm[${sm.rows}x${sm.columns}] vs jm[${jm.getRowDimension}x${jm.getColumnDimension}]")

  }

}

trait Verification {

  val littleValues: NArray[NArray[Double]] = NArray.tabulate[NArray[Double]](5)(
    (_: Int) => NArray.tabulate[Double](7)((_: Int) => Math.random())
  )

  val littleJaMa: Jama.Matrix = new Jama.Matrix(littleValues)
  val littleMa: slash.matrix.Matrix[5, 7] = slash.matrix.Matrix[5, 7](littleJaMa.getRowPackedCopy)

  val squareValues: NArray[NArray[Double]] = NArray.tabulate[NArray[Double]](11)(
    (_: Int) => NArray.tabulate[Double](11)((_: Int) => Math.random())
  )

  val wideValues: NArray[NArray[Double]] = NArray.tabulate[NArray[Double]](11)(
    (_: Int) => NArray.tabulate[Double](19)((_: Int) => Math.random())
  )

  val tallValues: NArray[NArray[Double]] = NArray.tabulate[NArray[Double]](21)(
    (_: Int) => NArray.tabulate[Double](12)((_: Int) => Math.random())
  )

  val squareJaMa: Jama.Matrix = new Jama.Matrix(squareValues)
  val squareMa: slash.matrix.Matrix[11, 11] = slash.matrix.Matrix[11, 11](squareJaMa.getRowPackedCopy)

  val wideJaMa: Jama.Matrix = new Jama.Matrix(wideValues)
  val wideMa: slash.matrix.Matrix[11, 19] = slash.matrix.Matrix[11, 19](wideJaMa.getRowPackedCopy)

  val tallJaMa: Jama.Matrix = new Jama.Matrix(tallValues)
  val tallMa: slash.matrix.Matrix[21, 12] = slash.matrix.Matrix[21, 12](tallJaMa.getRowPackedCopy)

  def name:String
  def run: Unit
  def verify: Unit = {
    println(s"Verifying ${Console.GREEN}$name${Console.RESET} against JAMA library.")
    run
    println(s"Verified ${Console.RED}$name${Console.RESET} against JAMA library.\n\n")
  }
}
