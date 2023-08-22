package verification.decomposition

import Jama.SingularValueDecomposition
import ai.dragonfly.math.matrix
import narr.*
import verification.Verification

import scala.Console.*

import scala.compiletime.ops.int.*

object SV extends Verification {

  override val name:String = "Singular Value Decomposition"

  override def run: Unit = {

    def compareJaMaToDragonflySVD[M <: Int, N <: Int](m: matrix.Matrix[M, N])(using ValueOf[M], ValueOf[N], M >= N =:= true): Unit = {
      val jsvd: SingularValueDecomposition = new SingularValueDecomposition(new Jama.Matrix(m.values))
      val msvd: matrix.decomposition.SV[M, N] = matrix.decomposition.SV[M, N](m)

      println(s"\tComparing Two norm condition number: ${jsvd.cond} vs ${msvd.cond} error = ${Math.abs(jsvd.cond() - msvd.cond)}")
      println(s"\tComparing Norm2: ${jsvd.norm2()} vs ${msvd.norm2} error = ${Math.abs(jsvd.norm2() - msvd.norm2)}")
      println(s"\tComparing Rank: ${jsvd.rank()} vs ${msvd.rank} error = ${Math.abs(jsvd.rank() - msvd.rank)}")


      println(s"\tComparing Singular Values: ${Verification.arrayCompare(jsvd.getSingularValues, msvd.singularValues.asInstanceOf[Array[Double]])}")

      println(s"\tComparing V : ${Verification.arrayCompare2D(jsvd.getV.getArray, msvd.V.values)}")
      println(s"\tComparing S : ${Verification.arrayCompare2D(jsvd.getS.getArray, msvd.S.values)}")
      println(s"\tComparing U : ${Verification.arrayCompare2D(jsvd.getU.getArray, msvd.U.values)}")

      println("\n")

      println(
        s"\tJaMa Comparing M to USVᵀ: ${
          Verification.arrayCompare2D(
            m.values,
            jsvd.getU().times(jsvd.getS()).times(jsvd.getV().transpose()).getArray
          )
        }"
      )

      println(
        s"\tdragonfly Comparing M to USVᵀ: ${
          Verification.arrayCompare2D(
            m.values,
            msvd.U.times(msvd.S).times(msvd.V.transpose).values
          )
        }"
      )
    }

    println("Compare Singular Value Decompositions on Square Matrix:")
    compareJaMaToDragonflySVD[11, 11](squareMa)

    println("Compare Singular Value Decompositions on Tall Matrix:")
    compareJaMaToDragonflySVD[21, 12](tallMa)

//    println("Compare Singular Value Decompositions on Wide Matrix:")
//    compareJaMaToDragonflySVD[11, 19](wideMa)

  }
}
