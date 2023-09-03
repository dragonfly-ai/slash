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

package ai.dragonfly.math

import ai.dragonfly.math.Random.nextVec
import ai.dragonfly.math.vector.Vec
import unicode.*
import narr.*

import scala.compiletime.ops.any.==
import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.int.*
import scala.collection.View

package object vector {

  opaque type Vec[N <: Int] = NArray[Double]
  object Vec {
    inline def apply[N <: Int](a: NArray[Double]): Vec[N] = { // cast a NArray[Double] as Vec[N]
      dimensionCheck(a, valueOf[N])
      a
    }

    inline def zeros[N <: Int](using ValueOf[N]): Vec[N] = fill[N](0.0)

    inline def ones[N <: Int](using ValueOf[N]): Vec[N] = fill[N](1.0)

    inline def random[N <: Int](
      MAX:Double = 1.0,
      min:Double = 0.0,
      r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom
    ): Vec[N] = r.nextVec[N](min, MAX)

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
      inline def rotate(cosTheta: Double, sinTheata: Double): Vec[2] = {
        val x1 = thisVector(0) * cosTheta - thisVector(1) * sinTheata
        thisVector(1) = thisVector(0) * sinTheata + thisVector(1) * cosTheta
        thisVector(0) = x1
        thisVector
      }
      inline def rotate(radians: Double): Vec[2] = rotate(Math.cos(radians), Math.sin(radians))
      inline def rotateDegrees(degrees: Double): Vec[2] = rotate(degreesToRadians(degrees))
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

      def copy:Vec[N] = {
        val copyOfThisVector:Vec[N] = new NArray[Double](dimension)
        var i = 0
        while (i < dimension) {
          copyOfThisVector(i) = thisVector(i)
          i = i + 1
        }
        copyOfThisVector
      }

      inline def mean: Double = {
        var sum = 0.0
        var i = 1
        while (i < dimension) {
          sum = sum + thisVector(i)
          i = i + 1
        }
        thisVector.sum / thisVector.size
      }
      //It is assumed, that we consider a sample rather than a complete population
      inline def variance: Double = {
        // https://www.cuemath.com/sample-variance-formula/
        val μ = thisVector.mean
        thisVector.map(i => squareInPlace(i - μ)).sum / (thisVector.size - 1)
      }

      // It is assumed, that we consider a sample rather than a complete population
      inline def stdDev: Double = {
        // https://www.cuemath.com/data/standard-deviation/
        val mu = thisVector.mean
        val diffs_2 = thisVector.map( num => squareInPlace(num - mu) )
        Math.sqrt( diffs_2.sum / (thisVector.size - 1 ) )
      }

      def covariance(thatVector : Vec[N] ) = {
        val μThis = thisVector.mean
        val μThat = thatVector.mean
        thisVector.zip(thatVector).map{ case (thisV, thatV) => (thisV - μThis) * (thatV - μThat) }.sum / (thisVector.size -1)
      }

      def pearsonCorrelationCoefficient(thatVector: Vec[N]): Double = {
        val n = thisVector.size
        val sum_x = thisVector.sum
        val sum_y = thatVector.sum
        val sum_xy = thisVector.zip(thatVector).map{ case (thisV, thatV) => thisV * thatV }.sum
        val sum_x2 = thisVector.map(squareInPlace(_)).sum
        val sum_y2 = thatVector.map(squareInPlace(_)).sum
        (n * sum_xy - (sum_x * sum_y)) / Math.sqrt( (sum_x2 * n - squareInPlace(sum_x)) * (sum_y2 * n - squareInPlace(sum_y)) )
      }

      def spearmansRankCorrelation(thatVector: Vec[N]): Double = {
        val theseRanks = thisVector.elementRanks
        val thoseRanks = thatVector.elementRanks
        val diffs = theseRanks - thoseRanks
        val diffs_2 = diffs.map(squareInPlace(_))
        val n = theseRanks.size
        val s = diffs_2.sum
        val numerator = 6 * diffs_2.sum
        val denominator = n * (squareInPlace(n) - 1)
        1 - ( numerator / denominator)
      }

      // An alias - pearson is the most commonly requested type of correlation
      def corr(thatVector: Vec[N]): Double = pearsonCorrelationCoefficient(thatVector)

      def elementRanks: Vec[N] = {
        val (sorted, originalPosition) = thisVector.zipWithIndex.toVector.sortBy(_._1).unzip
        val ranks : Vec[N] = NArray.tabulate[Double](thisVector.dimension)(i => (i+1).toDouble)

        var currentValue = sorted(0)
        var i = 0
        var currentSum = 0.0
        var currentCount = 0
        val resultList = List[Double]()

        for (value <- sorted) {
          if (value == currentValue) {
            currentSum += ranks(i)
            currentCount += 1
          } else {
            resultList = resultList ++ List.fill(currentCount)(currentSum / currentCount)
            currentValue = value
            currentCount = 1
            currentSum = ranks(i)
          }
          i = i + 1
        }
        resultList = resultList ++ List.fill(currentCount)(currentSum / currentCount)

        val rankResult : Vec[N] = new NArray[Double](thisVector.dimension)
        for( (idx, r) <- originalPosition.zip(resultList)) {
          rankResult(idx) = r
        }
        rankResult
      }

      inline def normSquared: Double = {
        var mag2 = 0.0
        var i = 0
        while (i < dimension) {
          mag2 = mag2 + squareInPlace(thisVector(i))
          i = i + 1
        }
        mag2
      }

      inline def norm: Double = Math.sqrt(normSquared)

      inline def magnitude: Double = norm

      inline def magnitudeSquared: Double = normSquared

      inline def euclideanDistanceSquaredTo(v0: Vec[N]): Double = {
        var distance = 0.0
        var i = 0
        while (i < dimension) {
          distance = distance + squareInPlace(thisVector(i) - v0(i))
          i = i + 1
        }
        distance
      }

      inline def euclideanDistanceTo(v0: Vec[N]): Double = Math.sqrt(euclideanDistanceSquaredTo(v0))

      inline def + (v0: Vec[N]): Vec[N] = copy.add(v0)

      inline def += (v0: Vec[N]): Vec[N] = add(v0)

      def add(v0: Vec[N]): Vec[N] = {
        var i = 0
        while (i < dimension) {
          thisVector(i) = thisVector(i) + v0(i)
          i = i + 1
        }
        thisVector
      }

      inline def unary_- : Vec[N] = thisVector * ( -1.0 )


      inline def -(v0: Vec[N]): Vec[N] = copy.subtract(v0)
      inline def -= (v0: Vec[N]): Vec[N] = subtract(v0)

      def subtract(v0: Vec[N]): Vec[N] = {
        var i = 0
        while (i < dimension) {
          thisVector(i) = thisVector(i) - v0(i)
          i = i + 1
        }
        thisVector
      }

      def dot(v0: Vec[N]): Double = {
        var product = 0.0
        var i = 0
        while (i < dimension) {
          product = product + thisVector(i) * v0(i)
          i = i + 1
        }
        product
      }

      inline def *(scalar: Double): Vec[N] = copy.scale(scalar)

      inline def *= (scalar: Double): Vec[N] = scale(scalar)
      inline def scale(scalar: Double): Vec[N] = {
        var i = 0
        while (i < dimension) {
          thisVector(i) = thisVector(i) * scalar
          i = i + 1
        }
        thisVector
      }
      inline def /(divisor: Double): Vec[N] = copy.divide(divisor)
      inline def /= (divisor: Double): Vec[N] = divide(divisor)

      inline def divide(divisor: Double): Vec[N] = scale(1.0 / divisor)

      inline def round(): Unit = {
        var i = 0
        while (i < dimension) {
          thisVector(i) = Math.round(thisVector(i)).toDouble
          i = i + 1
        }
      }

      inline def discretize(): Unit = round()

      inline def discretize(r: Double): Unit = {
        var i = 0
        while (i < dimension) {
          thisVector(i) = r * Math.round(thisVector(i) / r).toDouble
          i = i + 1
        }
      }

      inline def normalize(): Vec[N] = {
        val n: Double = norm
        if (n > 0.0) divide(n)
        else throw VectorNormalizationException(thisVector)
      }

      // ₂⃗ ²↗ ↗²
      def show: String = thisVector.dimension match {
        case 2 => s"《²↗〉${thisVector(0)}ᵢ ${thisVector(1)}ⱼ〉"
        case 3 => s"《³↗〉${thisVector(0)}ᵢ ${thisVector(1)}ⱼ ${thisVector(2)}ₖ〉"
        case 4 => s"《⁴↗〉${thisVector(0)}ᵢ ${thisVector(1)}ⱼ ${thisVector(2)}ₖ ${thisVector(3)}ₗ〉"
        case _ => render().toString()
      }

      def render(format:Format = Format.Default, sb: StringBuilder = new StringBuilder() ): StringBuilder = {
        import format.*
        sb.append(prefix(thisVector))
        val end:Int = dimension - 1
        var i = 0
        while (i < end) {
          sb.append(numberFormatter(thisVector(i)))
            .append(delimiter(i))
          i = i + 1
        }
        sb.append(numberFormatter(thisVector(dimension - 1)))
          .append(suffix(thisVector))
      }

      def csv(sb: StringBuilder = new StringBuilder()): String = render(Format.CSV, sb).toString
      def tsv(sb: StringBuilder = new StringBuilder()): String = render(Format.TSV, sb).toString
    }

    inline def fill[N <: Int](d: Double): Vec[N] = apply(NArray.fill[Double](valueOf[N])(d))

    inline def tabulate[N <: Int](f: Int => Double): Vec[N] = apply(NArray.tabulate[Double](valueOf[N])(f))

    def midpoint[N <: Int](v0: Vec[N], v1: Vec[N]): Vec[N] = (v0 + v1) * 0.5

    def blend[N <: Int](alpha: Double, v0: Vec[N], v1: Vec[N]):Vec[N] = (v0 * alpha) + (v1 * (1.0 - alpha))

    def mean[N <: Int](`[v₁v₂⋯vₙ]`: Vec[N]*): Vec[N] = {
      val μ: Vec[N] = `[v₁v₂⋯vₙ]`.head.copy
      for (v <- `[v₁v₂⋯vₙ]`.tail) {
        μ += v
      }
      μ /= `[v₁v₂⋯vₙ]`.size
    }

    def mean[N <: Int](`[v₀v₁⋯v₍ₙ₋₁₎]`: NArray[Vec[N]]): Vec[N] = {
      val μ: Vec[N] = `[v₀v₁⋯v₍ₙ₋₁₎]`(0).copy
      for (i <- 1 to `[v₀v₁⋯v₍ₙ₋₁₎]`.length) {
        μ += `[v₀v₁⋯v₍ₙ₋₁₎]`(i)
      }
      μ /= `[v₀v₁⋯v₍ₙ₋₁₎]`.length //.asInstanceOf[Vec[N]]
    }
  }

  trait Format {
    def prefix[N <: Int](v:Vec[N]): String
    def delimiter(index:Int): String
    def suffix[N <: Int](v:Vec[N]): String
    def numberFormatter(value: Double): String = value.toString
  }

  object Format {

    object Default extends Format {
      import Vec.*
      override def prefix[N <: Int](v: Vec[N]): String = s"《${exalt(v.dimension)}↗〉"
      override def delimiter(i: Int): String = ", "
      override def suffix[N <: Int](v: Vec[N]): String = "〉"
    }

    object Indexed extends Format {
      import Vec.*
      override def prefix[N <: Int](v: Vec[N]): String = s"《${exalt(v.dimension)}↗〉"
      override def delimiter(i: Int): String = s"${abase(i)} "
      override def suffix[N <: Int](v: Vec[N]): String = "〉"
    }

    object CSV extends Format {
      override def prefix[N <: Int](v: Vec[N]): String = ""
      override def delimiter(i: Int): String = ","
      override def suffix[N <: Int](v: Vec[N]): String = ""
    }

    object TSV extends Format {
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
