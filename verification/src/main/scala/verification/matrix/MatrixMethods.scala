package verification.matrix

import slash.matrix.*
import verification.*

object MatrixMethods extends Verification {
  override def name: String = "Mat Methods"

  def kitchenSink[M <: Int, N <: Int](j:Jama.Matrix, m: Mat[M, N])(using ValueOf[M], ValueOf[N]):Unit = {
    println("Comparing ...")
    println(s"\t- matrices : ${Verification.matrixCompare(j, m)}")
    println(s"\t- rowPackedCopy : ${Verification.arrayCompare(j.getRowPackedCopy, m.values)}")
    println(s"\t- columnPackedCopy : ${Verification.arrayCompare(j.getColumnPackedCopy, m.columnPackedArray)}")
    println(s"\t- columnPackedCopy : ${Verification.arrayCompare2D(j.getArray, m.rowVectors.asInstanceOf[Array[Array[Double]]])}")
    println(s"\t- transpose : ${Verification.matrixCompare(j.transpose(), m.transpose)}")
    println(s"\t- norm1 : ${j.norm1() == m.norm1}")
    println(s"\t- normInfinity : ${j.normInf() == m.normInfinity}")
    println(s"\t- normFrobenius : ${j.normF()} == ${m.normFrobenius} ${j.normF() == m.normFrobenius}")
    println(s"\t- Mat + Mat : ${Verification.matrixCompare(j.copy().plus(j), m + m)}")
    println(s"\t- Mat - Mat : ${Verification.matrixCompare(j.copy().minus(j), m - m)}")
    println(s"\t- -Mat : ${Verification.matrixCompare(j.copy().times(-1.0), -m)}")
    println(s"\t- MxMᵀ : ${Verification.matrixCompare(j.times(j.transpose()), m * m.transpose)}")
    println(s"\t- MᵀxM : ${Verification.matrixCompare(j.transpose().times(j), m.transpose * m)}")
    println(s"\t- trace : ${j.trace() == m.trace}")
    println(s"\t- subMatrix : ${Verification.matrixCompare(j.getMatrix(2, 4, 2, 4), m.subMatrix[3, 3](2, 2))}")

    val odd:Array[Int] = Array[Int](1,3,4,5)
    val even:Array[Int] = Array[Int](0,2,4,6)
    println(s"\t- subMatrix([],[]) : ${Verification.matrixCompare(j.getMatrix(odd, even), m.subMatrix[4,4](odd, even))}")
    println(s"\t- subMatrix([], 7) : ${Verification.matrixCompare(j.getMatrix(3, 7, odd), m.subMatrix[5,4](3, odd))}")
    println(s"\t- subMatrix(7, []) : ${Verification.matrixCompare(j.getMatrix(even, 2, 6), m.subMatrix[4,5](even, 2))}")

    // setMatrix

    // def setMatrix[M1 <: Int, N1 <: Int](r0: Int, c0: Int, thatMatrix: Mat[M1, N1])
    println(s"\t- setMatrix(1, 2, ${littleMa.dim}) : ${
      Verification.matrixCompare(
        {
          val t = j.copy()
          t.setMatrix(1, littleJaMa.getRowDimension, 2, littleJaMa.getColumnDimension + 1, littleJaMa)
          t
        },
        {
          val t = m.copy
          t.setMatrix(1, 2, littleMa)
          t
        }
      )
    }")

    val m4x4: Mat[4, 4] = Mat.random[4, 4]
    // def setMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], c0: Int, thatMatrix: Mat[M1, N1])
    println(s"\t- setMatrix([], 3, ${m4x4.dim}) : ${
      val jm4x4: Jama.Matrix = new Jama.Matrix(m4x4.rowVectors.asInstanceOf[Array[Array[Double]]])
      Verification.matrixCompare(
        {
          val t = j.copy()
          t.setMatrix(even, 3, 6, jm4x4)
          t
        },
        {
          val t = m.copy
          t.setMatrix(even, 3, m4x4)
          t
        }
      )
    }")
    // def setMatrix[M1 <: Int, N1 <: Int](r0: Int, columnIndices: NArray[Int], thatMatrix: Mat[M1, N1])
    println(s"\t- setMatrix(3, [], ${m4x4.dim}) : ${
      val jm4x4: Jama.Matrix = new Jama.Matrix(m4x4.rowVectors.asInstanceOf[Array[Array[Double]]])
      Verification.matrixCompare(
        {
          val t = j.copy()
          t.setMatrix(3, 6, odd, jm4x4)
          t
        },
        {
          val t = m.copy
          t.setMatrix(3, odd, m4x4)
          t
        }
      )
    }")

    // def setMatrix[M1 <: Int, N1 <: Int](rowIndices: NArray[Int], columnIndices: NArray[Int], thatMatrix: Mat[M1, N1])
    println(s"\t- setMatrix([], [], ${m4x4.dim}) : ${
      val jm4x4:Jama.Matrix = new Jama.Matrix(m4x4.rowVectors.asInstanceOf[Array[Array[Double]]])
      Verification.matrixCompare(
        {
          val t = j.copy()
          t.setMatrix(even, odd, jm4x4)
          t
        },
        {
          val t = m.copy
          t.setMatrix(even, odd, m4x4)
          t
        }
      )
    }")
  }

  override def run:Unit = {
    kitchenSink(squareJaMa, squareMa)
    kitchenSink(tallJaMa, tallMa)
    kitchenSink(wideJaMa, wideMa)

    println(s"\t- SQ x SQ : ${Verification.matrixCompare(squareJaMa.times(squareJaMa), squareMa * squareMa)}")
  }
}
