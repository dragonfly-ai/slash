/*
 * Copyright 2023 dragonfly.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package slash

import narr.*

import slash.*
import slash.vectorf.vectorf.*

import scala.compiletime.ops.int.*
import scala.compiletime.ops.any.==
import scala.compiletime.ops.boolean.||

package object matrixf {

  extension[N <: Int] (thisVector: VecF[N])(using ValueOf[N]) {
    inline def asRowMatrix: MatF[1, N] = MatF[1, N](thisVector.asInstanceOf[NArray[Float]])
    inline def asColumnMatrix: MatF[N, 1] = MatF[N, 1](thisVector.asInstanceOf[NArray[Float]])

    def times [M <: Int](thatMatrix: MatF[N, M])(using ValueOf[M]): MatF[1, M] = asRowMatrix * thatMatrix
    inline def * [M <: Int](thatMatrix: MatF[N, M])(using ValueOf[M]): MatF[1, M] = times(thatMatrix)
  }

  /**
   * Support left add / multiply by Scalars
   */
  extension(s: Float) {
    inline def +[M <: Int, N <: Int](inline m: MatF[M,N])(using ValueOf[M], ValueOf[N]): MatF[M,N] = m.copy.addScalar(s)
//  inline def *[M <: Int, N <: Int](inline m: MatF[M,N])(using ValueOf[M], ValueOf[N]): MatF[M,N] = m.copy.times(s)
  }

  /**
   * Extension methods for all matrices.
   */
  extension[M <: Int, N <: Int](a: MatF[M, N])(using ValueOf[M], ValueOf[N]) {

    /** cast matrix as MatF[R,C]
    *
    * @param R new vertical dimension
    * @param C new horizontal dimension
    * @return same values, but recast to RxC
    */
    def reshape[R <: Int, C <: Int](using ValueOf[R], ValueOf[C]): MatF[R,C] = new MatF[R,C](a.values)

    /** values as a Vector.
     */
    def flatten: VecF[M*N] = a.values.asInstanceOf[VecF[M*N]]

    def toMat: slash.matrix.Mat[M, N] = slash.matrix.Mat[M, N](
      NArray.tabulate[Double](valueOf[M] * valueOf[N])((i:Int) => a.values(i))
    )

  }

  /**
   * Extension Methods for Square Matrices.
   */
  extension [MN <: Int](m: MatF[MN, MN])(using ValueOf[MN]) {
    /**
     * https://en.wikipedia.org/wiki/Invertible_matrix
     *
     * Computes the inverse of Square MatF m.
     * @throws RuntimeException( "MatF is singular." )
     * @return the inverse of matrix m
     */
    def inv: MatF[MN, MN] = {
      //solve(MatF.identity[MN, MN])
      // convert to Mat, compute the inverse, then convert back
      import slash.matrix.*
      val dMat: slash.matrix.Mat[MN, MN] = m.toMat
      MatF.fromMat(dMat.inverse)
    }

    /** Solve a * x = b
     *
     * @param b right hand side
     * @return x = MatF[MN, V] such that a * x = b
     */
    def solve[V <: Int](b: MatF[MN, V])(using ValueOf[V]): MatF[MN, V] = {
      MatF.fromMat(slash.matrix.decomposition.LU[MN, MN](m.toMat).solve(b.toMat))
    }

    /** MatF determinant
     * https://en.wikipedia.org/wiki/Determinant
     * the determinant is nonzero if and only if the matrix is invertible and the linear map represented by the matrix is an isomorphism
     * @return the determinant of this matrix.
     */
    def determinant: Double = slash.matrix.decomposition.LU[MN, MN](m.toMat).determinant

  }


  /**
   * Extension methods for rectangular matrices.
   */
  extension[M <: Int, N <: Int](a: MatF[M, N])(using ValueOf[M], ValueOf[N], ValueOf[Min[M, N]], (N =:= M) =:= false) {
    /** Solve a * x = b
     *
     * @param b right hand side
     * @return least squares solution x = MatF[M, V] such that a * x = b
     */
    def solve[V <: Int](b: MatF[M, V])(using ValueOf[V]): MatF[N, V] = {
      MatF.fromMat(slash.matrix.decomposition.QR[M, N](a.toMat).solve(b.toMat))
    }
  }

  /**
   * Extension methods for rectangular matrices where M > N.
   */

  extension[M <: Int, N <: Int](m: MatF[M, N])(using ValueOf[M], ValueOf[N], ValueOf[Min[M, N]], M >= N =:= true) {
    /** Solve b * m = I[N, N]
     * m = MatF[M, N] with M > N and Rank = N, has a left inverse b = MatF[N, M] such that b * m = I[N, N]
     * @return b = MatF[N, M] the Left Inverse of MatF m.
     */
    def leftInverse: MatF[N, M] = {
      val svd = slash.matrix.decomposition.SV[M, N](m.toMat)
      MatF.fromMat(svd.V * svd.S_inverse * svd.U.transpose)
    }

    /** Two norm
     *
     * @return maximum singular value.
     */
    def norm2: Double = slash.matrix.decomposition.SV[M, N](m.toMat).norm2

    /** MatF rank
     *
     * @return effective numerical rank, obtained from SV.
     */
    def rank: Int = slash.matrix.decomposition.SV[M, N](m.toMat).rank

    /** MatF condition (2 norm)
     *
     * @return ratio of largest to smallest singular value.
     */
    def cond: Double = slash.matrix.decomposition.SV[M, N](m.toMat).cond

  }

  /**
   * Extension methods for rectangular matrices where M < N.
   */

  extension[M <: Int, N <: Int](m: MatF[M, N])(using ValueOf[M], ValueOf[N], ValueOf[Min[M, N]], N > M =:= true) {
    /**
     * m = MatF[M, N] with M < N and Rank = M, has a right inverse b = MatF[N, M] such that m * b = Identity[M, M]
     * @return the Right Inverse of MatF a.
     */
    def rightInverse(using ValueOf[Min[M, M]]): MatF[N, M] = {
      import slash.matrix.*
      MatF.fromMat(slash.matrix.decomposition.QR[M, N](m.toMat).solve(Mat.identity[M, M]))
    }

  }

  extension[M <: Int, N <: Int](m: MatF[M, N])(using (M == 1 || N == 1) =:= true) {
    def asVector: VecF[M*N] = m.values.asInstanceOf[VecF[M*N]]
    inline def copyAsVector[MN <: Int](using MN == (M * N) =:= true): VecF[MN] = narr.copy[Float](m.values).asInstanceOf[VecF[MN]]
  }

  /*
   * convert a MatF with non-literal dimensions to canonical form.
   */
  extension(m: MatF[? <: Int,? <: Int]) {
    def cast[M <: Int, N <: Int](using ValueOf[M], ValueOf[N]): MatF[M,N] = {
      assert(valueOf[M] == m.rows && valueOf[N] == m.columns)
      MatF[M,N](m.values)
    }
  }

}
