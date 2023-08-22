package verification.decomposition

import Jama.QRDecomposition
import ai.dragonfly.math.matrix
import narr.*
import verification.Verification

import scala.Console.*

object QR extends Verification {

  override val name:String = "QR Decomposition"

  override def run: Unit = {

    val jqr:QRDecomposition = new QRDecomposition(squareJaMa)
    val mqr:matrix.decomposition.QR[11, 11] = matrix.decomposition.QR[11, 11](squareMa)
    println("\tSquare Matrix[11, 11] decomposition:")
    println(s"\tComparing isFullRank: ${jqr.isFullRank} vs ${mqr.isFullRank}")
    println(s"\tComparing Q : ${Verification.arrayCompare2D(jqr.getQ.getArray, mqr.Q.values)}")
    println(s"\tComparing H : ${Verification.arrayCompare2D(jqr.getH.getArray, mqr.H.values)}")
    println(s"\tComparing R : ${Verification.arrayCompare2D(jqr.getR.getArray, mqr.R.values)}")

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
    println(s"\tComparing Q : ${Verification.arrayCompare2D(jqrTall.getQ.getArray, mqrTall.Q.values)}")
    println(s"\tComparing H : ${Verification.arrayCompare2D(jqrTall.getH.getArray, mqrTall.H.values)}")
    println(s"\tComparing R : ${Verification.arrayCompare2D(jqrTall.getR.getArray, mqrTall.R.values)}")
  }
}
