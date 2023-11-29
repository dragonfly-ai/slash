package verification.matrix.decomposition

import Jama.LUDecomposition
import slash.matrix
import verification.Verification

import scala.Console.*

object LU extends Verification {

  override val name:String = "LU Decomposition"

  override def run: Unit = {

    val jluSquare: LUDecomposition = new LUDecomposition(squareJaMa)
    val sluSquare: matrix.decomposition.LU[11, 11] = matrix.decomposition.LU[11, 11](squareMa)
    println("\tSquare Matrix[11, 11] decomposition:")
    println(s"\tComparing Determinants: ${jluSquare.det()} vs ${sluSquare.determinant} error = ${Math.abs(jluSquare.det() - sluSquare.determinant)}")
    println(s"\tComparing L : ${Verification.matrixCompare(jluSquare.getL, sluSquare.L)}")
    println(s"\tComparing U : ${Verification.matrixCompare(jluSquare.getU, sluSquare.U)}")

//    val jluWide: LUDecomposition = new LUDecomposition(wideJaMa)
//    val mluWide: matrix.decomposition.LU[11, 19] = matrix.decomposition.LU[11, 19](wideMa)
//    println(s"\tComparing Determinants: ${jluWide.det()} vs ${mluWide.det()} error = ${Math.abs(jluWide.det() - mluWide.det())}")
//    println(s"\tComparing L : ${Verification.arrayCompare2D(jluWide.getL.getArray, mluWide.getL().values)}")
//    println(s"\tComparing U : ${Verification.arrayCompare2D(jluWide.getU.getArray, mluWide.getU().values)}")

    val jluTall: LUDecomposition = new LUDecomposition(tallJaMa)
    val mluTall: matrix.decomposition.LU[21, 12] = matrix.decomposition.LU[21, 12](tallMa)
    println("\tTall Matrix[21, 12] decomposition:")
    println("\tRectangular matrices have no determinant.")
    println(s"\tComparing L : ${Verification.matrixCompare(jluTall.getL, mluTall.L)}")
    println(s"\tComparing U : ${Verification.matrixCompare(jluTall.getU, mluTall.U)}")
  }
}
