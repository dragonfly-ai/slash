package verification.matrix.decomposition


import Jama.{CholeskyDecomposition, Matrix}
import slash.Random.*

import slash.matrix
import narr.*
import verification.Verification

import scala.Console.*

object Cholesky extends Verification {

  override val name: String = "Cholesky Decomposition"
  override def run: Unit = {

    /*
     * Generate a symmetric positive definite matrix according to the discussion found here:
     * https://math.stackexchange.com/questions/357980/how-to-generate-random-symmetric-positive-definite-matrices-using-matlab
     */

    var spd: matrix.Mat[11, 11] = matrix.Mat.random[11, 11](-10.0, 10.0)
    spd = spd * spd.transpose + matrix.Mat.diagonal[11](defaultRandom.nextVec[11](2.0))

    val sqJCh: CholeskyDecomposition = new CholeskyDecomposition(new Matrix(spd.values, spd.rows))
    val sqMCh: matrix.decomposition.Cholesky[11] = matrix.decomposition.Cholesky[11](spd)

    println(s"\tComparing L : ${Verification.matrixCompare(sqJCh.getL, sqMCh.L)}")
  }
}
