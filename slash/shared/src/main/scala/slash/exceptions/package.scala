package slash

import slash.matrix.Mat

package object exceptions {

  case class UnsupportedVectorDimension(givenDimension: Int, requiredDimension: Int = -1) extends Exception(
    givenDimension match {
      case gd: Int if gd < 2 => s"Vector dimensions must exceed 1.  Cannot create a vector of dimension: $givenDimension"
      case _ => s"Expected Vector dimension: $requiredDimension, but observed: $givenDimension"
    }
  )

  case class VectorNormalizationException[N <: Int](s: String) extends Exception(s"Can't normalize $s")

  case class ExtraDimensionalAccessException[N <: Int](dim:Int, s:String, ci: Int) extends Exception({
    s"Index: $ci exceeds dimensionality of Euclidean object$dim: $s"
  })

  case class CannotExpressMatrixAsVector[M <: Int, N <: Int](m: Mat[M, N]) extends Exception(
    s"To convert a Matrix to a Vector, one of its dimensions must be 1, but this matrix has dimensions: [${m.rows}x${m.columns}]"
  )

  case class UnsupportedMatrixDimension(rows: Int, columns: Int) extends Exception(s"Can't create matrix with rows = $rows and columns = $columns.")

  case class MatrixNotSymmetricPositiveDefinite[M <: Int, N <: Int](m: Mat[M, N]) extends Exception(
    s"Matrix is not symmetric positive definite: Mat[${m.rows}x${m.columns}]."
  )
}
