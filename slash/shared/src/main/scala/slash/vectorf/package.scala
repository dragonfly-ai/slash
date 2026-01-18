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
import slash.interval.*
import slash.unicode.*
import slash.Random.nextVecF

import scala.language.implicitConversions
import scala.compiletime.ops.any.==
import scala.compiletime.ops.int.*

package object vectorf {

  opaque type VecF[N <: Int] = NArray[Float]

  object VecF {

    export narr.Extensions.given
    inline def apply[N <: Int](a: NArray[Float]): VecF[N] = { // cast a NArray[Float] as VecF[N]
      dimensionCheck(a.length, valueOf[N])
      a
    }

    inline def zeros[N <: Int](using ValueOf[N]): VecF[N] = new FloatArray(valueOf[N]).asInstanceOf[VecF[N]]

    inline def ones[N <: Int](using ValueOf[N]): VecF[N] = fill[N](1.0)

    inline def random[N <: Int]: VecF[N] = random(0.0, 1.0, slash.Random.defaultRandom)

    inline def random[N <: Int](MAX: Float):VecF[N] = random(0.0, MAX, slash.Random.defaultRandom)

    inline def random[N <: Int](min: Float, MAX: Float):VecF[N] = random(min, MAX, slash.Random.defaultRandom)

    inline def random[N <: Int](
      min:Float,
      MAX:Float,
      r:scala.util.Random
    ): VecF[N] = r.nextVecF[N](min, MAX)

    def random[N <: Int](
      interval: slash.interval.Interval[Float],
      r: scala.util.Random
    )(using ValueOf[N]): VecF[N] = VecF.tabulate[N](_ => interval.random(r))


    inline def apply(x: Float, y: Float): VecF[2] = NArray[Float](x, y)

    inline def apply(x: Float, y: Float, z: Float): VecF[3] = NArray[Float](x, y, z)

    inline def apply(x: Float, y: Float, z: Float, w: Float): VecF[4] = NArray[Float](x, y, z, w)

    /**
     * Varargs factory for high dimensional Vector literals.
     * Note: This is not an efficient way to create a vector.
     * @param d vector value literals
     * @tparam N intended vector dimension
     * @return a VecF[N] consisting of the specified Float valued literals.
     */
    inline def apply[N <: Int](d: Float*): VecF[N] = {
      val dimension:Int = valueOf[N]
      dimensionCheck(d.size, dimension)
      d.size match {
        case dim if dim < 2 => throw slash.exceptions.UnsupportedVectorDimension(dim)
        case 2 => apply(d(0), d(1))
        case 3 => apply(d(0), d(1), d(2))
        case 4 => apply(d(0), d(1), d(2), d(3))
        case _ => apply[dimension.type](NArray[Float](d: _*))
      }
    }

    inline def fill[N <: Int](d: Float): VecF[N] = apply(NArray.fill[Float](valueOf[N])(d))

    inline def tabulate[N <: Int](f: Int => Float): VecF[N] = apply(NArray.tabulate[Float](valueOf[N])(f))

    def midpoint[N <: Int](v0: VecF[N], v1: VecF[N]): VecF[N] = (v0 + v1) * 0.5

    def blend[N <: Int](alpha: Float, v0: VecF[N], v1: VecF[N]): VecF[N] = (v0 * alpha) + (v1 * (1.0f - alpha))

    def mean[N <: Int](`[v₁v₂⋯vₙ]`: VecF[N]*): VecF[N] = {
      val μ: VecF[N] = `[v₁v₂⋯vₙ]`.head.copy
      for (v <- `[v₁v₂⋯vₙ]`.tail) {
        μ += v
      }
      μ /= `[v₁v₂⋯vₙ]`.size.toFloat
      μ
    }

    def mean[N <: Int](`[v₀v₁⋯v₍ₙ₋₁₎]`: NArray[VecF[N]]): VecF[N] = {
      val μ: VecF[N] = `[v₀v₁⋯v₍ₙ₋₁₎]`(0).copy
      for (i <- 1 to `[v₀v₁⋯v₍ₙ₋₁₎]`.length) {
        μ += `[v₀v₁⋯v₍ₙ₋₁₎]`(i)
      }
      μ /= `[v₀v₁⋯v₍ₙ₋₁₎]`.length.toFloat //.asInstanceOf[VecF[N]]
      μ
    }

    private inline def DBL(a:Any):Float = a.asInstanceOf[Float]

    def fromTuple(t: (Float, Float)):VecF[2] = VecF[2](t._1, t._2)
    def fromTuple(t: (Float, Float, Float)):VecF[3] = VecF[3](t._1, t._2, t._3)
    def fromTuple(t: (Float, Float, Float, Float)):VecF[4] = VecF[4](t._1, t._2, t._3, t._4)
    def fromTuple(t: (Float, Float, Float, Float, Float)):VecF[5] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float)):VecF[6] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float)):VecF[7] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float)):VecF[8] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[9] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[10] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[11] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[12] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[13] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[14] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[15] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[16] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[17] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[18] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[19] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[20] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[21] = VecF.tabulate(i => DBL(t(i)))
    def fromTuple(t: (Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float)):VecF[22] = VecF.tabulate(i => DBL(t(i)))

    def fromVec[N <: Int](v:slash.vector.Vec[N]): VecF[N] = (NArray.tabulate[Float](v.dimension)(i => v(i).toFloat)).asInstanceOf[VecF[N]]

    extension[N <: Int] (thisVector: VecF[N])(using ValueOf[N], N >= 1 =:= true) {
      inline def x: Float = thisVector(0)
    }

    extension[N <: Int] (thisVector: VecF[N])(using ValueOf[N], N >= 2 =:= true) {
      inline def y: Float = thisVector(1)
    }

    extension[N <: Int] (thisVector: VecF[N])(using ValueOf[N], N >= 3 =:= true) {
      inline def z: Float = thisVector(2)
    }

    extension[N <: Int] (thisVector: VecF[N])(using ValueOf[N], N >= 4 =:= true) {
      inline def w: Float = thisVector(3)
    }

    /**
     * VecF[2] extension methods:
     */
    extension[N <: Int] (thisVector: VecF[N])(using ValueOf[N], N == 2 =:= true) {
      inline def rotate(cosTheta: Double, sinTheta: Double): Unit = {
        val x1 = thisVector(0) * cosTheta - thisVector(1) * sinTheta
        thisVector(1) = (thisVector(0) * sinTheta + thisVector(1) * cosTheta).toFloat
        thisVector(0) = x1.toFloat
      }
      inline def rotate(radians: Double): Unit = rotate(Math.cos(radians).toFloat, Math.sin(radians))
      inline def rotateDegrees(degrees: Double): Unit = rotate(degreesToRadians(degrees))
      inline def pseudoCross(thatVector: VecF[2]): Float = thisVector(0) * thatVector.y + thisVector(1) * thatVector.x

      /**
       * Compute the signed angle between two vectors.
       *
       * @param v the second vector to compare this vector to.
       * @return the signed angle in radians
       */
      inline def angleFrom(v: VecF[2]): Double = {
        //Math.acos( (thisVector dot v) / (thisVector.norm * v.norm) )  // unsigned method
        Math.atan2(thisVector.pseudoCross(v), thisVector dot v)
      }
    }

    /**
     * VecF[3] extension methods:
     */
    extension[N <: Int] (thisVector: VecF[N])(using ValueOf[N], N == 3 =:= true) {
      inline def ⨯(thatVector: VecF[3]): VecF[3] = cross(thatVector)

      inline def cross(thatVector: VecF[3]): VecF[3] = VecF[3](
        thisVector(1) * thatVector.z - thisVector(2) * thatVector.y, // u2*v3 - u3*v2,
        thisVector(2) * thatVector.x - thisVector(0) * thatVector.z, // u3*v1 - u1*v3,
        thisVector(0) * thatVector.y - thisVector(1) * thatVector.x // u1*v2 - u2*v1
      )

    }

    extension[N <: Int] (thisVector: VecF[N]) {

      inline def apply(index: Int): Float = thisVector(index)

      inline def update(index: Int, value: Float): Unit = thisVector(index) = value

      inline def dimension: Int = thisVector.length

      inline def copy:VecF[N] = thisVector.asInstanceOf[NArr[Float]].slice(0, dimension).asInstanceOf[VecF[N]]

      inline def asNativeArray: NArray[Float] = thisVector.asInstanceOf[NArray[Float]]

      inline def toVec: slash.vector.Vec[N] = slash.vector.Vec.fromVecF[N](thisVector)

      // clamp methods

      def clamp(lt: Float, gt: Float): Unit = {
        if (gt < lt) throw Exception(s"Invoked VecF[${thisVector.dimension}].clamp(lt = $lt, gt = $gt) but gt < lt.")
        var i = 0; while (i < thisVector.dimension) {
          if (thisVector(i) < lt) thisVector(i) = lt
          else if (thisVector(i) > gt) thisVector(i) = gt
          i = i + 1
        }
      }

      inline def clamp(i:Interval[Float]): Unit = clamp(i.set_min, i.set_MAX)

      inline def clamped(lt: Float, gt: Float):VecF[N] = {
        val o:VecF[N] = copy
        o.clamp(lt, gt)
        o
      }

      inline def clamped(i:Interval[Float]): VecF[N] = clamped(i.set_min, i.set_MAX)

      def min(lt: Float): Unit = {
        var i = 0; while (i < thisVector.dimension) {
          if (thisVector(i) < lt) thisVector(i) = lt
          i = i + 1
        }
      }

      def MAX(gt: Float): Unit = {
        var i = 0; while (i < thisVector.dimension) {
          if (thisVector(i) > gt) thisVector(i) = gt
          i = i + 1
        }
      }

      inline def clampedMin(lt: Float): VecF[N] = {
        val o:VecF[N] = copy
        o.min(lt)
        o
      }
      inline def clampedMAX(gt: Float): VecF[N] = {
        val o: VecF[N] = copy
        o.MAX(gt)
        o
      }


      def sum: Float = {
        var sum = 0.0
        var i = 0; while (i < thisVector.dimension) {
          sum = sum + thisVector(i)
          i = i + 1
        }
        sum.toFloat
      }

      inline def mean: Float = thisVector.sum / thisVector.dimension

      //It is assumed, that we consider a sample rather than a complete population
      def variance: Double = {
        // https://www.cuemath.com/sample-variance-formula/
        val μ = thisVector.mean
        thisVector.map(i => squareInPlace(i - μ)).sum / (thisVector.dimension - 1)
      }

      // It is assumed, that we consider a sample rather than a complete population
      def stdDev: Double = {
        // https://www.cuemath.com/data/standard-deviation/
        val mu = thisVector.mean
        val diffs_2 = thisVector.map( num => squareInPlace(num - mu) )
        Math.sqrt( diffs_2.sum / (thisVector.dimension - 1 ) )
      }

      def covariance(thatVector : VecF[N] ):Double = {
        val μThis = thisVector.mean
        val μThat = thatVector.mean
        var cv:Double = 0
        var i:Int = 0; while (i < thisVector.dimension) {
          cv += (thisVector(i) - μThis) * (thatVector(i) - μThat)
          i += 1
        }
        cv / (thisVector.dimension -1)
      }

      def normSquared: Double = {
        var mag2 = 0.0
        var i = 0; while (i < dimension) {
          mag2 = mag2 + squareInPlace(thisVector(i))
          i = i + 1
        }
        mag2
      }

      inline def norm: Double = Math.sqrt(normSquared)

      inline def magnitude: Double = norm

      inline def magnitudeSquared: Double = normSquared

      def euclideanDistanceSquaredTo(v0: VecF[N]): Double = {
        var distance = 0.0
        var i = 0; while (i < dimension) {
          distance = distance + squareInPlace(thisVector(i) - v0(i))
          i = i + 1
        }
        distance
      }

      inline def euclideanDistanceTo(v0: VecF[N]): Double = Math.sqrt(euclideanDistanceSquaredTo(v0))

      def + (scalar: Float): VecF[N] = {
        val vOut = copy
        var i = 0; while (i < dimension) {
          vOut(i) = vOut(i) + scalar
          i = i + 1
        }
        vOut
      }

      inline def +=(scalar: Float): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = thisVector(i) + scalar
          i = i + 1
        }
      }

      def + (v0: VecF[N]): VecF[N] = {
        val o:VecF[N] = copy
        o.add(v0)
        o
      }

      inline def += (v0: VecF[N]): Unit = add(v0)

      def add(v0: VecF[N]): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = thisVector(i) + v0(i)
          i = i + 1
        }
      }

      inline def unary_- : VecF[N] = thisVector * ( -1.0 )

      def - (scalar: Float): VecF[N] = {
        val vOut = copy
        var i = 0;while (i < dimension) {
          vOut(i) = vOut(i) - scalar
          i = i + 1
        }
        vOut
      }

      inline def -=(scalar: Float): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = thisVector(i) - scalar
          i = i + 1
        }
      }

      def -(v0: VecF[N]): VecF[N] = {
        val o: VecF[N] = copy
        o.subtract(v0)
        o
      }

      inline def -= (v0: VecF[N]): Unit = subtract(v0)

      def subtract(v0: VecF[N]): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = thisVector(i) - v0(i)
          i = i + 1
        }
      }

      def dot(v0: VecF[N]): Float = {
        var product = 0.0
        var i = 0; while (i < dimension) {
          product = product + thisVector(i) * v0(i)
          i = i + 1
        }
        product.toFloat
      }

      inline def * (scalar: Float): VecF[N] = scaled(scalar)

      inline def *= (scalar: Float): Unit = scale(scalar)
      def scale(scalar: Float): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = thisVector(i) * scalar
          i = i + 1
        }
      }

      def scaled(scalar: Float): VecF[N] = {
        val o: VecF[N] = copy
        o.scale(scalar)
        o
      }

      inline def / (divisor: Float): VecF[N] = divided(divisor)
      inline def /= (divisor: Float): Unit = divide(divisor)

      inline def divide(divisor: Float): Unit = scale(1.0f / divisor)

      def divided(divisor: Float): VecF[N] = {
        val o: VecF[N] = copy
        o.divide(divisor)
        o
      }

      def reciprocate: Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = 1.0f / thisVector(i)
          i = i + 1
        }
      }

      def reciprocal: VecF[N] = {
        val o: VecF[N] = copy
        o.reciprocate
        o
      }

      def pointwiseMultiply(v0: VecF[N]): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = thisVector(i) * v0(i)
          i = i + 1
        }
      }

      def pointwiseMultiplied(v0: VecF[N]): VecF[N] = {
        val o: VecF[N] = copy
        o.pointwiseMultiply(v0)
        o
      }

      def kronecker[M <: Int](v0: VecF[M])(using ValueOf[N * M]): VecF[N * M] = {
        val o: VecF[N * M] = VecF.zeros[N * M]
        var i = 0; while (i < dimension) {
          var j = 0; while (j < v0.dimension) {
            o(i * v0.dimension + j) = thisVector(i) * v0(j)
            j = j + 1
          }
          i = i + 1
        }
        o
      }

      def round(): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = Math.round(thisVector(i)).toFloat
          i = i + 1
        }
      }

      inline def rounded: VecF[N] = {
        val o: VecF[N] = copy
        o.round()
        o
      }

      inline def discretize(): Unit = round()

      def discretize(r: Float): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = r * Math.round(thisVector(i) / r).toFloat
          i = i + 1
        }
      }

      def discritized: VecF[N] = {
        val o: VecF[N] = copy
        o.round()
        o
      }

      def discritized(r: Float): VecF[N] = {
        val o: VecF[N] = copy
        o.discretize(r)
        o
      }

      inline def normalize(): Unit = {
        val n: Float = norm.toFloat
        if (n > 0.0) divide(n)
        else throw slash.exceptions.VectorNormalizationException(thisVector.render().toString())
      }

      def normalized: VecF[N] = {
        val o: VecF[N] = copy
        o.normalize()
        o
      }

      // ₂⃗ ²↗ ↗²
      def show: String = thisVector.dimension match {
        case 2 => s"《²↗〉${thisVector(0)}ᵢ ${thisVector(1)}ⱼ〉"
        case 3 => s"《³↗〉${thisVector(0)}ᵢ ${thisVector(1)}ⱼ ${thisVector(2)}ₖ〉"
        case 4 => s"《⁴↗〉${thisVector(0)}ᵢ ${thisVector(1)}ⱼ ${thisVector(2)}ₖ ${thisVector(3)}ₗ〉"
        case _ => render().toString()
      }

      def render(format:VecFFormat = VecFFormat.Default, sb: StringBuilder = new StringBuilder() ): StringBuilder = {
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
      def csv(sb: StringBuilder = new StringBuilder()): String = render(VecFFormat.CSV, sb).toString
      def tsv: String = tsv(new StringBuilder()).toString
      def tsv(sb: StringBuilder = new StringBuilder()): String = render(VecFFormat.TSV, sb).toString
    }

    extension (d: Float) {
      def *[N <: Int](v: VecF[N]):VecF[N] = v * d
    }

  }

  export VecF.*

  trait VecFFormat {
    def prefix[N <: Int](v:VecF[N]): String
    def delimiter(index:Int): String
    def suffix[N <: Int](v:VecF[N]): String
    def numberFormatter(value: Float): String = value.toString
  }

  object VecFFormat {

    object Default extends VecFFormat {
      override def prefix[N <: Int](v: VecF[N]): String = s"《${exalt(v.length)}↗〉"
      override def delimiter(i: Int): String = ", "
      override def suffix[N <: Int](v: VecF[N]): String = "〉"
    }

    object Indexed extends VecFFormat {
      override def prefix[N <: Int](v: VecF[N]): String = s"《${exalt(v.length)}↗〉"
      override def delimiter(i: Int): String = s"${abase(i)} "
      override def suffix[N <: Int](v: VecF[N]): String = "〉"
    }

    object CSV extends VecFFormat {
      override def prefix[N <: Int](v: VecF[N]): String = ""
      override def delimiter(i: Int): String = ","
      override def suffix[N <: Int](v: VecF[N]): String = ""
    }

    object TSV extends VecFFormat {
      override def prefix[N <: Int](v: VecF[N]): String = ""

      override def delimiter(i: Int): String = "\t"

      override def suffix[N <: Int](v: VecF[N]): String = ""
    }
  }

}
