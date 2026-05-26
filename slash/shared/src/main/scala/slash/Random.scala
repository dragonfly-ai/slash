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

import Constant.log2
import slash.vector.*
import Vec.*
import slash.vectorf.*
import VecF.*
import runtime.*
import RTVec.*
import narr.*
import slash.matrix.*

import java.math.MathContext

object Random {

  val defaultRandom:scala.util.Random = new scala.util.Random()

  extension(r:scala.util.Random)
  // Extension Methods for generating Random BigInt values
    private def bitCount(n: Int) = (n / log2).toInt
    private def digitCount(n: Int) = (n * log2).toInt
    def nextBigInt(precision: Int): BigInt = BigInt(bitCount(precision), r)
    def nextBigInt(norm: BigInt): BigInt = {
      val precision: Int = digitCount(norm.bitLength)
      (BigDecimal(norm)(new MathContext(precision + 1)) * BigDecimal(nextBigInt(precision))).toBigInt
    }
    def between(min: BigInt, MAX: BigInt): BigInt = min + r.nextBigInt(MAX - min)
    // Extension Methods for generating Random BigDecimal values
    def nextBigDecimal(precision: Int): BigDecimal = { val bd = BigDecimal( r.nextBigInt(precision) ); BigDecimal(bd.bigDecimal.movePointLeft(bd.precision)) }
    def nextBigDecimal(norm: BigDecimal, scale: Int): BigDecimal = norm * r.nextBigDecimal(scale)(new MathContext((norm.precision - norm.scale) + scale))
    def between(min: BigDecimal, MAX: BigDecimal): BigDecimal = min + r.nextBigDecimal(MAX - min, Math.max(min.precision, MAX.precision))

    inline def nextVec[N <: Int](maxNorm:Double = 1.0): Vec[N] = Vec.apply[N]( nextDoubles(valueOf[N], maxNorm) )
    inline def nextVec[N <: Int](minNorm:Double, normMAX:Double): Vec[N] = Vec.apply[N](
      nextDoubles(valueOf[N], minNorm, normMAX)
    )
    inline def nextVec[N <: Int](maxComponentNorm:Vec[N]):Vec[N] = Vec[N](
      nextDoubles(maxComponentNorm.asNativeArray)
    )
    inline def between[N <: Int](min:Vec[N], MAX:Vec[N]):Vec[N] = Vec.apply[N](
      betweenDoubles(min.asNativeArray, MAX.asNativeArray)
    )

    inline def nextVecF[N <: Int](maxNorm: Float = 1.0): VecF[N] = VecF.apply[N](
      NArray.tabulate[Float](valueOf[N])(_ => maxNorm * r.nextFloat())
    )
    inline def nextVecF[N <: Int](minNorm: Float, normMAX: Float): VecF[N] = VecF.apply[N](
      NArray.tabulate[Float](valueOf[N])(_ => r.between(minNorm, normMAX))
    )
    inline def nextVecF[N <: Int](maxComponentNorm: VecF[N]): VecF[N] = VecF[N](
      NArray.tabulate[Float](maxComponentNorm.dimension)((i: Int) => maxComponentNorm(i) * r.nextFloat())
    )
    inline def betweenF[N <: Int](min: VecF[N], MAX: VecF[N]): VecF[N] = VecF[N](
      NArray.tabulate[Float](min.dimension)((i: Int) => r.between(min(i), MAX(i)))
    )

    inline def nextRTVec(n: Int, maxNorm: Double = 1.0): RTVec = nextDoubles(n, maxNorm).asInstanceOf[RTVec]
    inline def nextRTVec(n: Int, minNorm: Double, normMAX: Double): RTVec = nextDoubles(n, minNorm, normMAX).asInstanceOf[RTVec]
    inline def nextRTVec(maxComponentNorm: RTVec): RTVec = nextDoubles(maxComponentNorm.asNativeArray).asInstanceOf[RTVec]
    inline def between(min: RTVec, MAX: RTVec): RTVec = {
      dimensionCheck(min.dimension, MAX.dimension)
      betweenDoubles(min.asNativeArray, MAX.asNativeArray).asInstanceOf[RTVec]
    }

    inline def nextDoubles(n: Int, maxNorm: Double = 1.0): NArray[Double] = {
      NArray.tabulate[Double](n)(_ => maxNorm * r.nextDouble())
    }
    inline def nextDoubles(n: Int, minNorm: Double, normMAX: Double): NArray[Double] = {
      NArray.tabulate[Double](n)(_ => r.between(minNorm, normMAX))
    }
    inline def nextDoubles(maxComponentNorm: NArray[Double]): NArray[Double] = {
      NArray.tabulate[Double](maxComponentNorm.length)((i: Int) => maxComponentNorm(i) * r.nextDouble())
    }
    inline def betweenDoubles(min: NArray[Double], MAX: NArray[Double]): NArray[Double] = {
      //assume correct dimensions.
      NArray.tabulate[Double](min.length)((i: Int) => r.between(min(i), MAX(i)))
    }

    inline def nextMatrix[M <: Int, N <: Int](maxNorm:Double = 1.0)(using ValueOf[M], ValueOf[N]): Mat[M, N] = Mat.random[M, N](0.0, maxNorm, r)
    inline def nextMatrix[M <: Int, N <: Int](minNorm:Double, normMAX:Double)(using ValueOf[M], ValueOf[N]): Mat[M, N] = Mat.random[M, N](minNorm, normMAX, r)

}
