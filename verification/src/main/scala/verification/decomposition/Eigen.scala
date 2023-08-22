package verification.decomposition

import Jama.EigenvalueDecomposition
import ai.dragonfly.math.matrix
import narr.*
import verification.Verification

import scala.Console.*

object Eigen extends Verification {

  override val name:String = "Eigen Decomposition"

  override def run: Unit = {

    val jed: Jama.EigenvalueDecomposition = new EigenvalueDecomposition(squareJaMa)
    val med: matrix.decomposition.Eigen[11] = matrix.decomposition.Eigen[11](squareMa)

    println(s"\tComparing Real Eigen Values: ${Verification.arrayCompare(jed.getRealEigenvalues(), med.realEigenvalues.asInstanceOf[Array[Double]])}")
    println(s"\tComparing Imaginary Eigen Values: ${Verification.arrayCompare(jed.getImagEigenvalues(), med.imaginaryEigenvalues.asInstanceOf[Array[Double]])}")

    println(s"\tComparing V : ${Verification.arrayCompare2D(jed.getV.getArray, med.Q.values)}")
    println(s"\tComparing D : ${Verification.arrayCompare2D(jed.getD.getArray, med.Λ.values)}")

  }
}