package verification.matrix.decomposition

import Jama.EigenvalueDecomposition
import slash.matrix
import verification.Verification

import scala.Console.*

object Eigen extends Verification {

  override val name:String = "Eigen Decomposition"

  override def run: Unit = {

    val jed: Jama.EigenvalueDecomposition = new EigenvalueDecomposition(squareJaMa)
    val med: matrix.decomposition.Eigen[11] = matrix.decomposition.Eigen[11](squareMa)

    println(s"\tComparing Real Eigen Values: ${Verification.arrayCompare(jed.getRealEigenvalues(), med.realEigenvalues.asInstanceOf[Array[Double]])}")
    println(s"\tComparing Imaginary Eigen Values: ${Verification.arrayCompare(jed.getImagEigenvalues(), med.imaginaryEigenvalues.asInstanceOf[Array[Double]])}")

    println(s"\tComparing V : ${Verification.matrixCompare(jed.getV, med.Q)}")
    println(s"\tComparing D : ${Verification.matrixCompare(jed.getD, med.Λ)}")

  }
}