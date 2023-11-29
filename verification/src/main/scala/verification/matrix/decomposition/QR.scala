package verification.matrix.decomposition

import Jama.QRDecomposition
import slash.matrix
import verification.Verification

import scala.Console.*

object QR extends Verification {

  override val name:String = "QR Decomposition"

  override def run: Unit = {

    val jqr:QRDecomposition = new QRDecomposition(squareJaMa)
    val mqr:matrix.decomposition.QR[11, 11] = matrix.decomposition.QR[11, 11](squareMa)
    println("\tSquare Matrix[11, 11] decomposition:")
    println(s"\tComparing isFullRank: ${jqr.isFullRank} vs ${mqr.isFullRank}")
    println(s"\tComparing Q : ${Verification.matrixCompare(mqr.Q, jqr.getQ)}")
    println(s"\tComparing H : ${Verification.matrixCompare( mqr.H, jqr.getH)}")
    println(s"\tComparing R : ${Verification.matrixCompare(mqr.R, jqr.getR)}")

//    val jqrWide: QRDecomposition = new QRDecomposition(wideJaMa)
//    val mqrWide: matrix.decomposition.QR[11, 19] = matrix.decomposition.QR[11, 19](wideMa)
//    println("\tWide Matrix[11, 19] decomposition:")
//    println(s"\tComparing isFullRank: ${jqrWide.isFullRank} vs ${mqrWide.isFullRank()}")
//    println(s"\tComparing Q : ${Verification.arrayCompare2D(jqrWide.getQ.getArray, mqrWide.Q.values)}")
//    println(s"\tComparing H : ${Verification.arrayCompare2D(jqrWide.getH.getArray, mqrWide.H.values)}")
//    println(s"\tComparing R : ${Verification.arrayCompare2D(jqrWide.getR.getArray, mqrWide.R.values)}")

    val jqrTall: QRDecomposition = new QRDecomposition(tallJaMa)
    val mqrTall: matrix.decomposition.QR[21, 12] = matrix.decomposition.QR[21, 12](tallMa)
    println("\tTall Matrix[21, 12] decomposition:")
    println(s"\tComparing isFullRank: ${jqrTall.isFullRank} vs ${mqrTall.isFullRank}")
    println(s"\tComparing Q : ${Verification.matrixCompare( mqrTall.Q, jqrTall.getQ)}")
    println(s"\tComparing H : ${Verification.matrixCompare(mqrTall.H, jqrTall.getH)}")
    println(s"\tComparing R : ${Verification.matrixCompare(mqrTall.R, jqrTall.getR)}")
  }
}
