package verification

import Jama.EigenvalueDecomposition
import ai.dragonfly.math.matrix
import narr.*
import verification.decomposition.*

import scala.Console.*

object Verify extends App {
  println("Testing matrix against original JAMA library.")
  Cholesky.verify
  Eigen.verify
  LU.verify
  QR.verify
  SV.verify
}