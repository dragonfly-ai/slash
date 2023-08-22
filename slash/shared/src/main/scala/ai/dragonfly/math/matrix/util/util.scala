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

package ai.dragonfly.math.matrix

import narr.*
import ai.dragonfly.math.vector.{Vec, *}
import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.decomposition.{SV, *}

import scala.compiletime.ops.int.*
package object util {

  extension[N <: Int] (thisVector: Vec[N])(using ValueOf[N]) {
    inline def asRowMatrix: Matrix[1, N] = Matrix[1, N](thisVector.asInstanceOf[NArray[Double]])
    inline def asColumnMatrix: Matrix[N, 1] = Matrix[N, 1](thisVector.asInstanceOf[NArray[Double]])

    def times [M <: Int] (thatMatrix: Matrix[N, M])(using ValueOf[M]): Matrix[1, M] = asRowMatrix * thatMatrix
    inline def * [M <: Int] (thatMatrix: Matrix[N, M])(using ValueOf[M]): Matrix[1, M] = times(thatMatrix)
  }

  /**
   * Extension Methods for Square Matrices.
   */
  extension [MN <: Int] (m: Matrix[MN, MN])(using ValueOf[MN]) {
    /**
     * https://en.wikipedia.org/wiki/Invertible_matrix
     *
     * Computes the inverse of Square Matrix m.
     * @throws RuntimeException( "Matrix is singular." )
     * @return the inverse of matrix m
     */
    def inverse: Matrix[MN, MN] = solve(Matrix.identity[MN, MN])

    /** Solve a * x = b
     *
     * @param b right hand side
     * @return x = Matrix[MN, V] such that a * x = b
     */
    def solve[V <: Int](b: Matrix[MN, V])(using ValueOf[V]): Matrix[MN, V] = LU[MN, MN](m).solve(b)

    /** Matrix determinant
     * https://en.wikipedia.org/wiki/Determinant
     * the determinant is nonzero if and only if the matrix is invertible and the linear map represented by the matrix is an isomorphism
     * @return the determinant of this matrix.
     */
    def determinant: Double = LU[MN, MN](m).determinant

  }


  /**
   * Extension methods for rectangular matrices.
   */
  extension[M <: Int, N <: Int] (a: Matrix[M, N])(using ValueOf[M], ValueOf[N], ValueOf[Min[M, N]], (N =:= M) =:= false) {
    /** Solve a * x = b
     *
     * @param b right hand side
     * @return least squares solution x = Matrix[M, V] such that a * x = b
     */
    def solve[V <: Int](b: Matrix[M, V])(using ValueOf[V]): Matrix[N, V] = QR[M, N](a).solve(b)
  }
  /**
   * Extension methods for rectangular matrices where M > N.
   */

  extension[M <: Int, N <: Int] (m: Matrix[M, N])(using ValueOf[M], ValueOf[N], ValueOf[Min[M, N]], M >= N =:= true) {
    /** Solve b * m = I[N, N]
     * m = Matrix[M, N] with M > N and Rank = N, has a left inverse b = Matrix[N, M] such that b * m = I[N, N]
     * @return b = Matrix[N, M] the Left Inverse of Matrix m.
     */
    def leftInverse: Matrix[N, M] = {
      val svd = SV[M, N](m)
      svd.V * svd.S_inverse * svd.U.transpose
    }


    /** Two norm
     *
     * @return maximum singular value.
     */
    def norm2: Double = SV[M, N](m).norm2

    /** Matrix rank
     *
     * @return effective numerical rank, obtained from SV.
     */
    def rank: Int = SV[M, N](m).rank

    /** Matrix condition (2 norm)
     *
     * @return ratio of largest to smallest singular value.
     */
    def cond: Double = SV[M, N](m).cond

  }

  /**
   * Extension methods for rectangular matrices where M < N.
   */

  extension[M <: Int, N <: Int] (m: Matrix[M, N])(using ValueOf[M], ValueOf[N], ValueOf[Min[M, N]], N > M =:= true) {
    /**
     * m = Matrix[M, N] with M < N and Rank = M, has a right inverse b = Matrix[N, M] such that m * b = Identity[M, M]
     * @return the Right Inverse of Matrix a.
     */
    def rightInverse(using ValueOf[Min[M, M]]): Matrix[N, M] = QR[M, N](m).solve(Matrix.identity[M, M])

  }

}
