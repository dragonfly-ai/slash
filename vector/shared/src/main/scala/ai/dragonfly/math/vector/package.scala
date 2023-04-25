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

import ai.dragonfly.math.vector.Vector
import unicode.*
import narr.*

import scala.compiletime.ops.int.*

package object vector {

  opaque type Vector[N <: Int] = NArray[Double]

  object Vector {
    inline def apply[N <: Int](a: NArray[Double]): Vector[N] = { // sneaky way to cast an NArray[Double] to a Vector[N]
      dimensionCheck(a, valueOf[N])
      a
    }

    inline def random[N <: Int](maxNorm:Double = 1.0): Vector[N] = {
      import ai.dragonfly.math.Random.*
      defaultRandom.nextVector[N](maxNorm)
    }

    inline def apply(x: Double, y: Double): Vector[2] = NArray[Double](x, y)

    inline def apply(x: Double, y: Double, z: Double): Vector[3] = NArray[Double](x, y, z)

    inline def apply(x: Double, y: Double, z: Double, w: Double): Vector[4] = NArray[Double](x, y, z, w)

    /**
     * Varargs factory for high dimensional Vector literals.
     * Note: This is not an efficient way to create a vector.
     * @param d vector value literals
     * @tparam N intended vector dimension
     * @return a Vector[N] consisting of the specified Double valued literals.
     */
    inline def apply[N <: Int](d: Double*): Vector[N] = {
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

    extension[N <: Int] (thisVector: Vector[N]) {

      inline def apply(index: Int): Double = thisVector(index)

      inline def update(index: Int, value: Double): Unit = thisVector(index) = value

      inline def dimension: Int = thisVector.length

      def copy:Vector[N] = {
        val copyOfThisVector:Vector[N] = new NArray[Double](dimension)
        var i = 0
        while (i < dimension) {
          copyOfThisVector(i) = thisVector(i)
          i = i + 1
        }
        copyOfThisVector
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

      inline def euclideanDistanceSquaredTo(v0: Vector[N]): Double = {
        var distance = 0.0
        var i = 0
        while (i < dimension) {
          distance = distance + squareInPlace(thisVector(i) - v0(i))
          i = i + 1
        }
        distance
      }

      inline def euclideanDistanceTo(v0: Vector[N]): Double = Math.sqrt(euclideanDistanceSquaredTo(v0))

      inline def +(v0: Vector[N]): Vector[N] = copy.add(v0)

      inline def += (v0: Vector[N]): Vector[N] = add(v0)

      def add(v0: Vector[N]): Vector[N] = {
        var i = 0
        while (i < dimension) {
          thisVector(i) = thisVector(i) + v0(i)
          i = i + 1
        }
        thisVector
      }


      inline def -(v0: Vector[N]): Vector[N] = copy.subtract(v0)
      inline def -= (v0: Vector[N]): Vector[N] = subtract(v0)

      def subtract(v0: Vector[N]): Vector[N] = {
        var i = 0
        while (i < dimension) {
          thisVector(i) = thisVector(i) - v0(i)
          i = i + 1
        }
        thisVector
      }

      def dot(v0: Vector[N]): Double = {
        var product = 0.0
        var i = 0
        while (i < dimension) {
          product = product + thisVector(i) * v0(i)
          i = i + 1
        }
        product
      }

      inline def *(scalar: Double): Vector[N] = copy.scale(scalar)

      inline def *= (scalar: Double): Vector[N] = scale(scalar)
      inline def scale(scalar: Double): Vector[N] = {
        var i = 0
        while (i < dimension) {
          thisVector(i) = thisVector(i) * scalar
          i = i + 1
        }
        thisVector
      }
      inline def /(divisor: Double): Vector[N] = copy.divide(divisor)
      inline def /= (divisor: Double): Vector[N] = divide(divisor)

      inline def divide(divisor: Double): Vector[N] = scale(1.0 / divisor)

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

      inline def normalize(): Vector[N] = {
        val n: Double = norm
        if (n > 0.0) divide(n)
        else throw VectorNormalizationException(thisVector)
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


    inline def fill[N <: Int](d: Double): Vector[N] = apply(NArray.fill[Double](valueOf[N])(d))

    inline def tabulate[N <: Int](f: Int => Double): Vector[N] = apply(NArray.tabulate[Double](valueOf[N])(f))

    def midpoint[N <: Int](v0: Vector[N], v1: Vector[N]): Vector[N] = (v0 + v1) * 0.5

    def blend[N <: Int](alpha: Double, v0: Vector[N], v1: Vector[N]):Vector[N] = (v0 * alpha) + (v1 * (1.0 - alpha))

    def mean[N <: Int](`[v₁v₂⋯vₙ]`: Vector[N]*): Vector[N] = {
      val μ: Vector[N] = `[v₁v₂⋯vₙ]`.head.copy
      for (v <- `[v₁v₂⋯vₙ]`.tail) {
        μ += v
      }
      μ /= `[v₁v₂⋯vₙ]`.size
    }

    def mean[N <: Int](`[v₀v₁⋯v₍ₙ₋₁₎]`: NArray[Vector[N]]): Vector[N] = {
      val μ: Vector[N] = `[v₀v₁⋯v₍ₙ₋₁₎]`(0).copy
      for (i <- 1 to `[v₀v₁⋯v₍ₙ₋₁₎]`.length) {
        μ += `[v₀v₁⋯v₍ₙ₋₁₎]`(i)
      }
      μ /= `[v₀v₁⋯v₍ₙ₋₁₎]`.length //.asInstanceOf[Vector[N]]
    }
  }

  trait Format {
    def prefix[N <: Int](v:Vector[N]): String
    def delimiter(index:Int): String
    def suffix[N <: Int](v:Vector[N]): String
    def numberFormatter(value: Double): String = value.toString
  }

  object Format {

    object Default extends Format {
      import Vector.*
      override def prefix[N <: Int](v: Vector[N]): String = s"《${exalt(v.dimension)}↗〉"
      override def delimiter(i: Int): String = ", "
      override def suffix[N <: Int](v: Vector[N]): String = "〉"
    }

    object Indexed extends Format {
      import Vector.*
      override def prefix[N <: Int](v: Vector[N]): String = s"《${exalt(v.dimension)}↗〉"
      override def delimiter(i: Int): String = s"${abase(i)} "
      override def suffix[N <: Int](v: Vector[N]): String = "〉"
    }

    object CSV extends Format {
      override def prefix[N <: Int](v: Vector[N]): String = ""
      override def delimiter(i: Int): String = ","
      override def suffix[N <: Int](v: Vector[N]): String = ""
    }

    object TSV extends Format {
      override def prefix[N <: Int](v: Vector[N]): String = ""

      override def delimiter(i: Int): String = "\t"

      override def suffix[N <: Int](v: Vector[N]): String = ""
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


case class VectorNormalizationException[N <: Int](v:Vector[N]) extends Exception(s"Can't normalize ${v.show}")

case class ExtraDimensionalAccessException[N <: Int](v:Vector[N], ci: Int) extends Exception(
  s"Index: $ci exceeds dimensionality of Euclidean object${v.dimension}: ${v.show}"
)