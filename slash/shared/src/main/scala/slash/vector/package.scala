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
import slash.interval.*
import slash.unicode.*

import scala.language.implicitConversions
import scala.compiletime.ops.any.==
import scala.compiletime.ops.int.*

package object vector {

  opaque type Vec[N <: Int] = NArray[Double]

  object Vec {

    export narr.Extensions.given
    inline def apply[N <: Int](a: NArray[Double]): Vec[N] = { // cast a NArray[Double] as Vec[N]
      dimensionCheck(a, valueOf[N])
      a
    }

    inline def zeros[N <: Int]: Vec[N] = util.zeros(valueOf[N])

    inline def ones[N <: Int]: Vec[N] = util.ones(valueOf[N])

    inline def random[N <: Int]: Vec[N] = util.random(valueOf[N])

    inline def random[N <: Int](MAX: Double):Vec[N] = util.random(valueOf[N], MAX)

    inline def random[N <: Int](min: Double, MAX: Double):Vec[N] = util.random(valueOf[N], min, MAX)

    inline def random[N <: Int](
      min:Double,
      MAX:Double,
      r:scala.util.Random
    ): Vec[N] = util.random(valueOf[N], min, MAX, r)

    def random[N <: Int](
      interval: slash.interval.Interval[Double],
      r: scala.util.Random
    )(using ValueOf[N]): Vec[N] = util.random(valueOf[N], interval, r)


    inline def apply(x: Double, y: Double): Vec[2] = NArray[Double](x, y)

    inline def apply(x: Double, y: Double, z: Double): Vec[3] = NArray[Double](x, y, z)

    inline def apply(x: Double, y: Double, z: Double, w: Double): Vec[4] = NArray[Double](x, y, z, w)

    /**
     * Varargs factory for high dimensional Vector literals.
     * Note: This is not an efficient way to create a vector.
     * @param d vector value literals
     * @tparam N intended vector dimension
     * @return a Vec[N] consisting of the specified Double valued literals.
     */
    inline def apply[N <: Int](d: Double*): Vec[N] = {
      val dimension:Int = valueOf[N]
      dimensionCheck(d.size, dimension)
      d.size match {
        case dim if dim < 2 => throw slash.exceptions.UnsupportedVectorDimension(dim)
        case 2 => apply(d(0), d(1))
        case 3 => apply(d(0), d(1), d(2))
        case 4 => apply(d(0), d(1), d(2), d(3))
        case _ => apply[dimension.type](NArray[Double](d: _*))
      }
    }

    inline def fill[N <: Int](d: Double): Vec[N] = apply(NArray.fill[Double](valueOf[N])(d))

    inline def tabulate[N <: Int](f: Int => Double): Vec[N] = apply(NArray.tabulate[Double](valueOf[N])(f))

    def midpoint[N <: Int](v0: Vec[N], v1: Vec[N]): Vec[N] = (v0 + v1) * 0.5

    def blend[N <: Int](alpha: Double, v0: Vec[N], v1: Vec[N]): Vec[N] = (v0 * alpha) + (v1 * (1.0 - alpha))

    def mean[N <: Int](vs: Vec[N]*): Vec[N] = util.mean(vs: _*)

    def mean[N <: Int](vs: NArray[Vec[N]]): Vec[N] = util.mean(vs)
//    {
//      val μ: Vec[N] = `[v₀v₁⋯v₍ₙ₋₁₎]`(0).copy
//      for (i <- 1 to `[v₀v₁⋯v₍ₙ₋₁₎]`.length) {
//        μ += `[v₀v₁⋯v₍ₙ₋₁₎]`(i)
//      }
//      μ /= `[v₀v₁⋯v₍ₙ₋₁₎]`.length //.asInstanceOf[Vec[N]]
//      μ
//    }

    private inline def DBL(a:Any):Double = a.asInstanceOf[Double]

    def fromTuple(t: (Double, Double)):Vec[2] = Vec[2](t._1, t._2)
    def fromTuple(t: (Double, Double, Double)):Vec[3] = Vec[3](t._1, t._2, t._3)
    def fromTuple(t: (Double, Double, Double, Double)):Vec[4] = Vec[4](t._1, t._2, t._3, t._4)
    def fromTuple(t: (Double, Double, Double, Double, Double)):Vec[5] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double)):Vec[6] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double)):Vec[7] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double)):Vec[8] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[9] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[10] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[11] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[12] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[13] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[14] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[15] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[16] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[17] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[18] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[19] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[20] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[21] = Vec.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)):Vec[22] = Vec.tabulate(i => DBL(t(i)))

    def fromVecF[N <: Int](v:slash.vectorf.VecF[N]): Vec[N] = (NArray.tabulate[Double](v.dimension)(i => v(i).toDouble)).asInstanceOf[Vec[N]]

    extension[N <: Int] (thisVector: Vec[N])(using N >= 1 =:= true) {
      inline def x: Double = thisVector(0)
    }

    extension[N <: Int] (thisVector: Vec[N])(using N >= 2 =:= true) {
      inline def y: Double = thisVector(1)
    }

    extension[N <: Int] (thisVector: Vec[N])(using N >= 3 =:= true) {
      inline def z: Double = thisVector(2)
    }

    extension[N <: Int] (thisVector: Vec[N])(using N >= 4 =:= true) {
      inline def w: Double = thisVector(3)
    }

    /**
     * Vec[2] extension methods:
     */
    extension[N <: Int] (thisVector: Vec[N])(using N == 2 =:= true) {
      inline def rotate(cosTheta: Double, sinTheta: Double): Unit = {
        val x1 = thisVector(0) * cosTheta - thisVector(1) * sinTheta
        thisVector(1) = thisVector(0) * sinTheta + thisVector(1) * cosTheta
        thisVector(0) = x1
      }
      inline def rotate(radians: Double): Unit = rotate(Math.cos(radians), Math.sin(radians))
      inline def rotateDegrees(degrees: Double): Unit = rotate(degreesToRadians(degrees))
      inline def pseudoCross(thatVector: Vec[2]): Double = thisVector(0) * thatVector.y + thisVector(1) * thatVector.x

      /**
       * Compute the signed angle between two vectors.
       *
       * @param v the second vector to compare this vector to.
       * @return the signed angle in radians
       */
      inline def angleFrom(v: Vec[2]): Double = {
        //Math.acos( (thisVector dot v) / (thisVector.norm * v.norm) )  // unsigned method
        Math.atan2(thisVector.pseudoCross(v), thisVector dot v)
      }
    }

    /**
     * Vec[3] extension methods:
     */
    extension[N <: Int] (thisVector: Vec[N])(using N == 3 =:= true) {
      inline def ⨯(thatVector: Vec[3]): Vec[3] = cross(thatVector)

      inline def cross(thatVector: Vec[3]): Vec[3] = Vec[3](
        thisVector(1) * thatVector.z - thisVector(2) * thatVector.y, // u2*v3 - u3*v2,
        thisVector(2) * thatVector.x - thisVector(0) * thatVector.z, // u3*v1 - u1*v3,
        thisVector(0) * thatVector.y - thisVector(1) * thatVector.x // u1*v2 - u2*v1
      )

    }

    extension[N <: Int] (thisVector: Vec[N]) {

      inline def apply(index: Int): Double = thisVector(index)

      inline def update(index: Int, value: Double): Unit = thisVector(index) = value

      inline def dimension: Int = thisVector.length

      inline def copy:Vec[N] = thisVector.asInstanceOf[NArr[Double]].slice(0, dimension).asInstanceOf[Vec[N]]

      inline def asNativeArray: NArray[Double] = thisVector.asInstanceOf[NArray[Double]]

      inline def toVecF: slash.vectorf.VecF[N] = slash.vectorf.VecF.fromVec[N](thisVector)

      // clamp methods

      inline def clamp(lt: Double, gt: Double): Unit = util.clamp(thisVector, lt, gt)

      inline def clamp(i:Interval[Double]): Unit = util.clamp(thisVector, i)

      inline def clamped(lt: Double, gt: Double):Vec[N] = util.clamped(thisVector, lt, gt)

      inline def clamped(i:Interval[Double]): Vec[N] = clamped(i.set_min, i.set_MAX)

      inline def min(lt: Double): Unit = util.min(thisVector, lt)

      inline def MAX(gt: Double): Unit = util.MAX(thisVector, gt)

      inline def clampedMin(lt: Double): Vec[N] = util.clampedMin(thisVector, lt)

      inline def clampedMAX(gt: Double): Vec[N] = util.clampedMAX(thisVector, gt)

      inline def sum: Double = util.sum(thisVector)

      inline def mean: Double = util.mean(thisVector)

      //It is assumed, that we consider a sample rather than a complete population
      inline def variance: Double = util.variance(thisVector)

      // It is assumed, that we consider a sample rather than a complete population
      inline def stdDev: Double = util.stdDev(thisVector)

      inline def covariance(thatVector : Vec[N] ):Double = util.covariance(thisVector, thatVector)

      inline def normSquared: Double = util.normSquared(thisVector)

      inline def norm: Double = util.norm(thisVector)

      inline def magnitude: Double = util.norm(thisVector)

      inline def magnitudeSquared: Double = util.normSquared(thisVector)

      inline def euclideanDistanceSquaredTo(thatVector: Vec[N]): Double = util.euclideanDistanceSquaredBetween(thisVector, thatVector)

      inline def euclideanDistanceTo(thatVector: Vec[N]): Double = util.euclideanDistanceBetween(thisVector, thatVector)

      inline def + (scalar: Double): Vec[N] = util.scalarCopyAndAdd(thisVector, scalar)

      inline def +=(scalar: Double): Unit = util.scalarAddInPlace(thisVector, scalar)

      inline def + (thatVector: Vec[N]): Vec[N] = util.vectorCopyAndAdd(thisVector, thatVector)

      inline def += (thatVector: Vec[N]): Unit = util.vectorAddInPlace(thisVector, thatVector)

      inline def add(thatVector: Vec[N]): Unit = util.vectorAddInPlace(thisVector, thatVector)

      inline def unary_- : Vec[N] = thisVector * ( -1.0 )

      inline def - (scalar: Double): Vec[N] = util.scalarCopyAndSubtract(thisVector, scalar)

      inline def -=(scalar: Double): Unit = util.scalarSubtractInPlace(thisVector, scalar)

      inline def -(thatVector: Vec[N]): Vec[N] = util.vectorCopyAndSubtract(thisVector, thatVector)

      inline def -= (thatVector: Vec[N]): Unit = util.vectorSubtractInPlace(thisVector, thatVector)

      inline def subtract(thatVector: Vec[N]): Unit = util.vectorSubtractInPlace(thisVector, thatVector)

      inline def dot(thatVector: Vec[N]): Double = util.dot(thisVector, thatVector)

      inline def * (scalar: Double): Vec[N] = util.scaled(thisVector, scalar)
      inline def *= (scalar: Double): Unit = util.scaleInPlace(thisVector, scalar)

      inline def scale(scalar: Double): Unit = util.scaleInPlace(thisVector, scalar)

      inline def scaled(scalar: Double): Vec[N] = util.scaled(thisVector, scalar)

      inline def / (divisor: Double): Vec[N] = util.divided(thisVector, divisor)
      inline def /= (divisor: Double): Unit = util.divideInPlace(thisVector, divisor)

      inline def divide(divisor: Double): Unit = util.divideInPlace(thisVector, divisor)

      inline def divided(divisor: Double): Vec[N] = util.divided(thisVector, divisor)

      inline def reciprocate(): Unit = util.reciprocate(thisVector)

      inline def reciprocal: Vec[N] = util.reciprocal(thisVector)

      inline def pointwiseMultiply(thatVector: Vec[N]): Unit = util.pointwiseMultiply(thisVector, thatVector)

      inline def pointwiseMultiplied(thatVector: Vec[N]): Vec[N] = util.pointwiseMultiplied(thisVector, thatVector)

      inline def kronecker[M <: Int](thatVector: Vec[M]): Vec[N * M] = util.kronecker(thisVector, thatVector)

      inline def round(): Unit = util.round(thisVector)

      inline def rounded: Vec[N] = util.rounded(thisVector)

      inline def discretize(): Unit = util.round(thisVector)

      inline def discretize(r: Double): Unit = util.discretize(thisVector, r)

      inline def discritized: Vec[N] = util.discritized(thisVector)

      inline def discritized(r: Double): Vec[N] = util.discritized(thisVector, r)

      inline def normalize(): Unit = util.normalize(thisVector)

      inline def normalized: Vec[N] = util.normalized(thisVector)

      // ₂⃗ ²↗ ↗²
      def show: String = thisVector.dimension match {
        case 2 => s"《²↗〉${thisVector(0)}ᵢ ${thisVector(1)}ⱼ〉"
        case 3 => s"《³↗〉${thisVector(0)}ᵢ ${thisVector(1)}ⱼ ${thisVector(2)}ₖ〉"
        case 4 => s"《⁴↗〉${thisVector(0)}ᵢ ${thisVector(1)}ⱼ ${thisVector(2)}ₖ ${thisVector(3)}ₗ〉"
        case _ => render().toString()
      }

      def render(format:VecFormat = VecFormat.Default, sb: StringBuilder = new StringBuilder() ): StringBuilder = {
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
      def csv(sb: StringBuilder = new StringBuilder()): String = render(VecFormat.CSV, sb).toString
      def tsv: String = tsv(new StringBuilder()).toString
      def tsv(sb: StringBuilder = new StringBuilder()): String = render(VecFormat.TSV, sb).toString
    }

    extension (d: Double) {
      def *[N <: Int](v: Vec[N]):Vec[N] = v * d
    }

  }

  export Vec.*

  trait VecFormat {
    def prefix[N <: Int](v:Vec[N]): String
    def delimiter(index:Int): String
    def suffix[N <: Int](v:Vec[N]): String
    def numberFormatter(value: Double): String = value.toString
  }

  object VecFormat {

    object Default extends VecFormat {
      override def prefix[N <: Int](v: Vec[N]): String = s"《${exalt(v.length)}↗〉"
      override def delimiter(i: Int): String = ", "
      override def suffix[N <: Int](v: Vec[N]): String = "〉"
    }

    object Indexed extends VecFormat {
      override def prefix[N <: Int](v: Vec[N]): String = s"《${exalt(v.length)}↗〉"
      override def delimiter(i: Int): String = s"${abase(i)} "
      override def suffix[N <: Int](v: Vec[N]): String = "〉"
    }

    object CSV extends VecFormat {
      override def prefix[N <: Int](v: Vec[N]): String = ""
      override def delimiter(i: Int): String = ","
      override def suffix[N <: Int](v: Vec[N]): String = ""
    }

    object TSV extends VecFormat {
      override def prefix[N <: Int](v: Vec[N]): String = ""

      override def delimiter(i: Int): String = "\t"

      override def suffix[N <: Int](v: Vec[N]): String = ""
    }
  }

}
