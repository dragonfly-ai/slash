package verification

import verification.matrix.decomposition.*
import verification.matrix.*

import scala.Console.*

object Verify extends App {
  println("Testing matrix against original JAMA library.")
  MatrixMethods.verify
  Cholesky.verify
  Eigen.verify
  LU.verify
  QR.verify
  SV.verify
}