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

import slash.Random.nextVec
import slash.vector.*
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

    inline def zeros[N <: Int](using ValueOf[N]): Vec[N] = new DoubleArray(valueOf[N]).asInstanceOf[Vec[N]]

    inline def ones[N <: Int](using ValueOf[N]): Vec[N] = fill[N](1.0)

    inline def random[N <: Int]: Vec[N] = random(0.0, 1.0, slash.Random.defaultRandom)

    inline def random[N <: Int](MAX: Double):Vec[N] = random(0.0, MAX, slash.Random.defaultRandom)

    inline def random[N <: Int](min: Double, MAX: Double):Vec[N] = random(min, MAX, slash.Random.defaultRandom)

    inline def random[N <: Int](
      min:Double,
      MAX:Double,
      r:scala.util.Random
    ): Vec[N] = r.nextVec[N](min, MAX)

    def random[N <: Int](
      interval: slash.interval.Interval[Double],
      r: scala.util.Random
    )(using ValueOf[N]): Vec[N] = Vec.tabulate[N](_ => interval.random(r))


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
        case dim if dim < 2 => throw UnsupportedVectorDimension(dim)
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

    def mean[N <: Int](`[v₁v₂⋯vₙ]`: Vec[N]*): Vec[N] = {
      val μ: Vec[N] = `[v₁v₂⋯vₙ]`.head.copy
      for (v <- `[v₁v₂⋯vₙ]`.tail) {
        μ += v
      }
      μ /= `[v₁v₂⋯vₙ]`.size
      μ
    }

    def mean[N <: Int](`[v₀v₁⋯v₍ₙ₋₁₎]`: NArray[Vec[N]]): Vec[N] = {
      val μ: Vec[N] = `[v₀v₁⋯v₍ₙ₋₁₎]`(0).copy
      for (i <- 1 to `[v₀v₁⋯v₍ₙ₋₁₎]`.length) {
        μ += `[v₀v₁⋯v₍ₙ₋₁₎]`(i)
      }
      μ /= `[v₀v₁⋯v₍ₙ₋₁₎]`.length //.asInstanceOf[Vec[N]]
      μ
    }

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

    private val rankVariableSort: Ordering[(Double, Int)] = Ordering.by(_._1)

    extension[N <: Int] (thisVector: Vec[N])(using ValueOf[N], N >= 1 =:= true) {
      inline def x: Double = thisVector(0)
    }

    extension[N <: Int] (thisVector: Vec[N])(using ValueOf[N], N >= 2 =:= true) {
      inline def y: Double = thisVector(1)
    }

    extension[N <: Int] (thisVector: Vec[N])(using ValueOf[N], N >= 3 =:= true) {
      inline def z: Double = thisVector(2)
    }

    extension[N <: Int] (thisVector: Vec[N])(using ValueOf[N], N >= 4 =:= true) {
      inline def w: Double = thisVector(3)
    }

    /**
     * Vec[2] extension methods:
     */
    extension[N <: Int] (thisVector: Vec[N])(using ValueOf[N], N == 2 =:= true) {
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
    extension[N <: Int] (thisVector: Vec[N])(using ValueOf[N], N == 3 =:= true) {
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


      // clamp methods

      def clamp(lt: Double, gt: Double): Unit = {
        if (gt < lt) throw Exception(s"Invoked Vec[${thisVector.dimension}].clamp(lt = $lt, gt = $gt) but gt < lt.")
        var i = 0; while (i < thisVector.dimension) {
          if (thisVector(i) < lt) thisVector(i) = lt
          else if (thisVector(i) > gt) thisVector(i) = gt
          i = i + 1
        }
      }

      inline def clamp(i:Interval[Double]): Unit = clamp(i.set_min, i.set_MAX)

      inline def clamped(lt: Double, gt: Double):Vec[N] = {
        val o:Vec[N] = copy
        o.clamp(lt, gt)
        o
      }

      inline def clamped(i:Interval[Double]): Vec[N] = clamped(i.set_min, i.set_MAX)

      def min(lt: Double): Unit = {
        var i = 0; while (i < thisVector.dimension) {
          if (thisVector(i) < lt) thisVector(i) = lt
          i = i + 1
        }
      }

      def MAX(gt: Double): Unit = {
        var i = 0; while (i < thisVector.dimension) {
          if (thisVector(i) > gt) thisVector(i) = gt
          i = i + 1
        }
      }

      inline def clampedMin(lt: Double): Vec[N] = {
        val o:Vec[N] = copy
        o.min(lt)
        o
      }
      inline def clampedMAX(gt: Double): Vec[N] = {
        val o: Vec[N] = copy
        o.MAX(gt)
        o
      }


      def sum: Double = {
        var sum = 0.0
        var i = 0; while (i < thisVector.dimension) {
          sum = sum + thisVector(i)
          i = i + 1
        }
        sum
      }

      inline def mean: Double = thisVector.sum / thisVector.dimension

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

      def covariance(thatVector : Vec[N] ):Double = {
        val μThis = thisVector.mean
        val μThat = thatVector.mean
        var cv:Double = 0
        var i:Int = 0; while (i < thisVector.dimension) {
          cv += (thisVector(i) - μThis) * (thatVector(i) - μThat)
          i += 1
        }
        cv / (thisVector.dimension -1)
      }

      def pearsonCorrelationCoefficient(thatVector: Vec[N]): Double = {
        val n = thisVector.dimension
        var i = 0

        var sum_x = 0.0
        var sum_y = 0.0
        var sum_xy = 0.0
        var sum_x2 = 0.0
        var sum_y2 = 0.0

        while (i < n) {
          sum_x = sum_x + thisVector(i)
          sum_y = sum_y + thatVector(i)
          sum_xy = sum_xy + thisVector(i) * thatVector(i)
          sum_x2 = sum_x2 + squareInPlace(thisVector(i))
          sum_y2 = sum_y2 + squareInPlace(thatVector(i))
          i = i + 1
        }
        (n * sum_xy - (sum_x * sum_y)) / Math.sqrt( (sum_x2 * n - squareInPlace(sum_x)) * (sum_y2 * n - squareInPlace(sum_y)) )
      }

      def spearmansRankCorrelation(thatVector: Vec[N]) : Double = {
        val theseRanks = thisVector.elementRanks
        val thoseRanks = thatVector.elementRanks
        theseRanks.pearsonCorrelationCoefficient(thoseRanks)
      }


      // An alias - pearson is the most commonly requested type of correlation
      inline def corr(thatVector: Vec[N]): Double = pearsonCorrelationCoefficient(thatVector)

      def elementRanks: Vec[N] = {
        val indexed:NArray[(Double, Int)] = thisVector.zipWithIndex
        indexed.sort(rankVariableSort)

        val ranks : Vec[N] = new DoubleArray(thisVector.dimension) // faster than zeros.
        ranks(indexed.last._2) = thisVector.dimension
        var currentValue:Double = indexed(0)._1
        var r0:Int = 0
        var rank:Int = 1
        while (rank < thisVector.dimension) {
          val temp: Double = indexed(rank)._1
          val end:Int = {
            if (temp != currentValue) rank
            else if (rank == thisVector.dimension - 1) rank + 1
            else -1
          }
          if (end > -1) {
            val avg: Double = (1.0 + (end + r0)) / 2.0
            var i:Int = r0; while (i < end) {
              ranks(indexed(i)._2) = avg
              i += 1
            }
            r0 = rank
            currentValue = temp
          }
          rank += 1
        }
        ranks
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

      def euclideanDistanceSquaredTo(v0: Vec[N]): Double = {
        var distance = 0.0
        var i = 0; while (i < dimension) {
          distance = distance + squareInPlace(thisVector(i) - v0(i))
          i = i + 1
        }
        distance
      }

      inline def euclideanDistanceTo(v0: Vec[N]): Double = Math.sqrt(euclideanDistanceSquaredTo(v0))

      def + (scalar: Double): Vec[N] = {
        val vOut = copy
        var i = 0; while (i < dimension) {
          vOut(i) = vOut(i) + scalar
          i = i + 1
        }
        vOut
      }

      inline def +=(scalar: Double): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = thisVector(i) + scalar
          i = i + 1
        }
      }

      def + (v0: Vec[N]): Vec[N] = {
        val o:Vec[N] = copy
        o.add(v0)
        o
      }

      inline def += (v0: Vec[N]): Unit = add(v0)

      def add(v0: Vec[N]): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = thisVector(i) + v0(i)
          i = i + 1
        }
      }

      inline def unary_- : Vec[N] = thisVector * ( -1.0 )

      def - (scalar: Double): Vec[N] = {
        val vOut = copy
        var i = 0;while (i < dimension) {
          vOut(i) = vOut(i) - scalar
          i = i + 1
        }
        vOut
      }

      inline def -=(scalar: Double): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = thisVector(i) - scalar
          i = i + 1
        }
      }

      def -(v0: Vec[N]): Vec[N] = {
        val o: Vec[N] = copy
        o.subtract(v0)
        o
      }

      inline def -= (v0: Vec[N]): Unit = subtract(v0)

      def subtract(v0: Vec[N]): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = thisVector(i) - v0(i)
          i = i + 1
        }
      }

      def dot(v0: Vec[N]): Double = {
        var product = 0.0
        var i = 0; while (i < dimension) {
          product = product + thisVector(i) * v0(i)
          i = i + 1
        }
        product
      }

      inline def * (scalar: Double): Vec[N] = scaled(scalar)

      inline def *= (scalar: Double): Unit = scale(scalar)
      def scale(scalar: Double): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = thisVector(i) * scalar
          i = i + 1
        }
      }

      def scaled(scalar: Double): Vec[N] = {
        val o: Vec[N] = copy
        o.scale(scalar)
        o
      }

      inline def / (divisor: Double): Vec[N] = divided(divisor)
      inline def /= (divisor: Double): Unit = divide(divisor)

      inline def divide(divisor: Double): Unit = scale(1.0 / divisor)

      def divided(divisor: Double): Vec[N] = {
        val o: Vec[N] = copy
        o.divide(divisor)
        o
      }

      def round(): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = Math.round(thisVector(i)).toDouble
          i = i + 1
        }
      }

      inline def rounded: Vec[N] = {
        val o: Vec[N] = copy
        o.round()
        o
      }

      inline def discretize(): Unit = round()

      def discretize(r: Double): Unit = {
        var i = 0; while (i < dimension) {
          thisVector(i) = r * Math.round(thisVector(i) / r).toDouble
          i = i + 1
        }
      }

      def discritized: Vec[N] = {
        val o: Vec[N] = copy
        o.round()
        o
      }

      def discritized(r: Double): Vec[N] = {
        val o: Vec[N] = copy
        o.discretize(r)
        o
      }

      inline def normalize(): Unit = {
        val n: Double = norm
        if (n > 0.0) divide(n)
        else throw VectorNormalizationException(thisVector)
      }

      def normalized: Vec[N] = {
        val o: Vec[N] = copy
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

  inline def dimensionCheck(supplied:Int, required: Int): Unit = {
    if (supplied != required) throw UnsupportedVectorDimension(supplied, required)
  }

  inline def dimensionCheck(values:NArray[Double], requiredDimension: Int): NArray[Double] = {
    dimensionCheck(values.length, requiredDimension)
    values
  }

}

case class UnsupportedVectorDimension(givenDimension:Int, requiredDimension:Int = -1) extends Exception(
  givenDimension match {
    case gd:Int if gd < 2 => s"Vector dimensions must exceed 1.  Cannot create a vector of dimension: $givenDimension"
    case _ => s"Expected Vector dimension: $requiredDimension, but observed: $givenDimension"
  }
)


case class VectorNormalizationException[N <: Int](v:Vec[N]) extends Exception({
  import vector.*
  import Vec.*
  s"Can't normalize ${v.render()}"
})

case class ExtraDimensionalAccessException[N <: Int](v:Vec[N], ci: Int) extends Exception({
  import vector.*
  import Vec.*
  s"Index: $ci exceeds dimensionality of Euclidean object${v.dimension}: ${v.render()}"
})
