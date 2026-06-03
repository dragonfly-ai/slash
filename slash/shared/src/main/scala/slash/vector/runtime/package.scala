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

package slash.vector

import narr.*
import slash.dimensionCheck
import slash.interval.*
import slash.unicode.*

import scala.language.implicitConversions

package object runtime {

  opaque type RTVec = NArray[Double]

  object RTVec {

    export narr.Extensions.given
    inline def apply(a: NArray[Double]): RTVec = a // cast a NArray[Double] as RTVec

    inline def zeros(n: Int): RTVec = util.zeros(n)

    inline def ones(n: Int): RTVec = util.ones(n)

    inline def random(n: Int): RTVec = util.random(n)

    inline def random(n: Int, MAX: Double):RTVec = util.random(n, MAX)

    inline def random(n: Int, min: Double, MAX: Double):RTVec = util.random(n, min, MAX)

    inline def random(
      n: Int,
      min:Double,
      MAX:Double,
      r:scala.util.Random
    ): RTVec = util.random(n, min, MAX, r) //r.nextVecN(n, min, MAX)

    def random(
      n: Int,
      interval: slash.interval.Interval[Double],
      r: scala.util.Random
    ): RTVec = util.random(n, interval, r)

    /**
     * Varargs factory for high dimensional Vector literals.
     * Note: This is not an efficient way to create a vector.
     * @param d vector value literals
     * @return a RTVec consisting of the specified Double valued literals.
     */
    inline def apply(dimension: Int, d: Double*): RTVec = {
      dimensionCheck(d.size, dimension)
      apply(NArray[Double](d: _*))
    }

    inline def fill(n:Int, d: Double): RTVec = util.fill(n, d)

    inline def tabulate(n: Int, f: Int => Double): RTVec = util.tabulate(n, f)

    inline def midpoint(v0: RTVec, v1: RTVec): RTVec = util.midpoint(v0, v1)

    inline def blend(alpha: Double, v0: RTVec, v1: RTVec): RTVec = util.blend(alpha, v0, v1)

    def mean(vs: RTVec*): RTVec = {
      val d: Int = vs.head.length
      for (v <- vs.tail) {
        dimensionCheck(d, v.dimension)
      }
      util.mean(vs: _*)
    }

    def mean(vs: NArray[RTVec]): RTVec = {
      val d: Int = vs(0).length
      for (i <- 1 to vs.length) {
        dimensionCheck(d, vs(i).dimension)
      }
      util.mean(vs)
    }

    extension(thisVector: RTVec) {

      inline def apply(index: Int): Double = thisVector(index)

      inline def update(index: Int, value: Double): Unit = thisVector(index) = value

      inline def dimension: Int = thisVector.length

      inline def copy:RTVec = util.copy(thisVector)

      inline def asNativeArray: NArray[Double] = thisVector

//      inline def toVecF: slash.vectorf.VecF[N] = slash.vectorf.VecF.fromVecN(thisVector)

      // clamp methods

      inline def clamp(lt: Double, gt: Double): Unit = util.clamp(thisVector, lt, gt)

      inline def clamp(i:Interval[Double]): Unit = util.clamp(thisVector, i)

      inline def clamped(lt: Double, gt: Double):RTVec = util.clamped(thisVector, lt, gt)

      inline def clamped(i:Interval[Double]): RTVec = util.clamped(thisVector, i.set_min, i.set_MAX)

      inline def min(lt: Double): Unit = util.min(thisVector, lt)

      inline def MAX(gt: Double): Unit = util.MAX(thisVector, gt)

      inline def clampedMin(lt: Double): RTVec = util.clampedMin(thisVector, lt)

      inline def clampedMAX(gt: Double): RTVec = util.clampedMAX(thisVector, gt)

      inline def sum: Double = util.sum(thisVector)

      inline def mean: Double = util.mean(thisVector)

      //It is assumed, that we consider a sample rather than a complete population
      inline def variance: Double = util.variance(thisVector)

      // It is assumed, that we consider a sample rather than a complete population
      inline def stdDev: Double = util.stdDev(thisVector)

      inline def covariance(thatVector : RTVec ):Double = {
        dimensionCheck(dimension, thatVector.dimension)
        util.covariance(thisVector, thatVector)
      }

      inline def normSquared: Double = util.normSquared(thisVector)

      inline def norm: Double = util.norm(thisVector)

      inline def magnitude: Double = util.norm(thisVector)

      inline def magnitudeSquared: Double = util.magnitudeSquared(thisVector)

      inline def euclideanDistanceSquaredTo(thatVector: RTVec): Double = {
        dimensionCheck(dimension, thatVector.dimension)
        util.euclideanDistanceSquaredBetween(thisVector, thatVector)
      }

      inline def euclideanDistanceTo(thatVector: RTVec): Double = {
        dimensionCheck(dimension, thatVector.dimension)
        util.euclideanDistanceBetween(thisVector, thatVector)
      }

      inline def + (scalar: Double): RTVec = util.scalarCopyAndAdd(thisVector, scalar)

      inline def +=(scalar: Double): Unit = util.scalarAddInPlace(thisVector, scalar)

      inline def + (thatVector: RTVec): RTVec = {
        dimensionCheck(dimension, thatVector.dimension)
        util.vectorCopyAndAdd(thisVector, thatVector)
      }

      inline def += (thatVector: RTVec): Unit = {
        dimensionCheck(dimension, thatVector.dimension)
        util.vectorAddInPlace(thisVector, thatVector)
      }

      inline def add(thatVector: RTVec): Unit = {
        dimensionCheck(dimension, thatVector.dimension)
        util.vectorAddInPlace(thisVector, thatVector)
      }

      inline def unary_- : RTVec = util.scaled(thisVector, -1.0) //thisVector * ( -1.0 )

      inline def - (scalar: Double): RTVec = util.scalarCopyAndSubtract(thisVector, scalar)

      inline def -= (scalar: Double): Unit = util.scalarSubtractInPlace(thisVector, scalar)

      inline def -(thatVector: RTVec): RTVec = {
        dimensionCheck(dimension, thatVector.dimension)
        util.vectorCopyAndSubtract(thisVector, thatVector)
      }

      inline def -= (thatVector: RTVec): Unit = {
        dimensionCheck(dimension, thatVector.dimension)
        util.vectorSubtractInPlace(thisVector, thatVector)
      }

      inline def subtract(thatVector: RTVec): Unit = {
        dimensionCheck(dimension, thatVector.dimension)
        util.vectorSubtractInPlace(thisVector, thatVector)
      }

      inline def dot(thatVector: RTVec): Double = {
        dimensionCheck(dimension, thatVector.dimension)
        util.dot(thisVector, thatVector)
      }

      inline def * (scalar: Double): RTVec = util.scaled(thisVector, scalar)

      inline def *= (scalar: Double): Unit = util.scaleInPlace(thisVector, scalar)

      inline def scale(scalar: Double): Unit = util.scaleInPlace(thisVector, scalar)

      inline def scaled(scalar: Double): RTVec = util.scaled(thisVector, scalar)

      inline def / (divisor: Double): RTVec = util.divided(thisVector, divisor)
      inline def /= (divisor: Double): Unit = util.divideInPlace(thisVector, divisor)

      inline def divide(divisor: Double): Unit = util.divideInPlace(thisVector, 1.0 / divisor)

      inline def divided(divisor: Double): RTVec = util.divided(thisVector, divisor)

      inline def reciprocate(): Unit = util.reciprocate(thisVector)

      inline def reciprocal: RTVec = util.reciprocal(thisVector)

      inline def pointwiseMultiply(thatVector: RTVec): Unit = {
        dimensionCheck(dimension, thatVector.dimension)
        util.pointwiseMultiply(thisVector, thatVector)
      }

      inline def pointwiseMultiplied(thatVector: RTVec): RTVec = {
        dimensionCheck(dimension, thatVector.dimension)
        util.pointwiseMultiplied(thisVector, thatVector)
      }

      inline def kronecker(thatVector: RTVec): RTVec = {
        dimensionCheck(dimension, thatVector.dimension)
        util.kronecker(thisVector, thatVector)
      }

      inline def round(): Unit = util.round(thisVector)

      inline def rounded: RTVec = util.rounded(thisVector)

      inline def discretize(): Unit = util.round(thisVector)

      inline def discretize(r: Double): Unit = util.discretize(thisVector, r)

      inline def discritized: RTVec = util.discritized(thisVector)

      inline def discritized(r: Double): RTVec = util.discritized(thisVector, r)

      inline def normalize(): Unit = util.normalize(thisVector)

      inline def normalized: RTVec = util.normalized(thisVector)

      // ₂⃗ ²↗ ↗²
      def show: String = render().toString()

      def render(format:VecNFormat = VecNFormat.Default, sb: StringBuilder = new StringBuilder() ): StringBuilder = {
        import format.*
        sb.append(prefix(thisVector))
        val end:Int = dimension - 1
        var i = 0; while (i < end) {
          sb.append(numberFormatter(thisVector(i)))
            .append(delimiter(i))
          i = i + 1
        }
        sb.append(numberFormatter(thisVector(dimension - 1)))
          .append(suffix(thisVector))
      }

      def csv: String = csv(new StringBuilder()).toString
      def csv(sb: StringBuilder = new StringBuilder()): String = render(VecNFormat.CSV, sb).toString
      def tsv: String = tsv(new StringBuilder()).toString
      def tsv(sb: StringBuilder = new StringBuilder()): String = render(VecNFormat.TSV, sb).toString
    }

    extension (d: Double) {
      def *(v: RTVec):RTVec = v * d
    }

  }

  export RTVec.*

  trait VecNFormat {
    def prefix(v:RTVec): String
    def delimiter(index:Int): String
    def suffix(v:RTVec): String
    def numberFormatter(value: Double): String = value.toString
  }

  object VecNFormat {

    object Default extends VecNFormat {
      override def prefix(v: RTVec): String = s"《${exalt(v.length)}↗〉"
      override def delimiter(i: Int): String = ", "
      override def suffix(v: RTVec): String = "〉"
    }

    object Indexed extends VecNFormat {
      override def prefix(v: RTVec): String = s"《${exalt(v.length)}↗〉"
      override def delimiter(i: Int): String = s"${abase(i)} "
      override def suffix(v: RTVec): String = "〉"
    }

    object CSV extends VecNFormat {
      override def prefix(v: RTVec): String = ""
      override def delimiter(i: Int): String = ","
      override def suffix(v: RTVec): String = ""
    }

    object TSV extends VecNFormat {
      override def prefix(v: RTVec): String = ""

      override def delimiter(i: Int): String = "\t"

      override def suffix(v: RTVec): String = ""
    }
  }

}
