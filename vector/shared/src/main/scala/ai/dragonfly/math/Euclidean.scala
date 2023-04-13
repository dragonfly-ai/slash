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

import ai.dragonfly.math.vector.{VectorN, dimensionCheck}

trait Euclidean {
  val self:Euclidean = this

  def dimension: Int

  inline def inDimensionOrThrowException(i:Int):Unit = if (i < 0 || dimension <= i) throw ExtraDimensionalAccessException(self, i)

  inline def sameDimensionOrThrowException(that:Euclidean):Unit = dimensionCheck(self.dimension, that.dimension)

  // read
  def component(i: Int): Double

  // write
  def component(i: Int, d:Double): Double  // returns d if successful, -1 on failure

  object euclid {

    inline def normSquared: Double = {
      var mag2 = 0.0
      var i = 0
      while(i < dimension) {
        mag2 = mag2 + squareInPlace(component(i))
        i = i + 1
      }
      mag2
    }

    inline def norm: Double = Math.sqrt(normSquared)

    inline def distanceSquaredTo(that: Euclidean): Double = {
      sameDimensionOrThrowException(that)

      var distance = 0.0
      var i = 0
      while(i < dimension) {
        distance = distance + squareInPlace(self.component(i) - that.component(i))
        i = i + 1
      }
      distance
    }

    inline def distanceTo(that: Euclidean): Double = Math.sqrt(distanceSquaredTo(that))

    inline def dot(that: Euclidean): Double = {
      sameDimensionOrThrowException(that)
      var product = 0.0
      var i = 0
      while(i < dimension) {
        product = product + self.component(i) * that.component(i)
        i = i + 1
      }
      product
    }


    inline def scale(scalar: Double): Unit = {
      var i = 0
      while(i < dimension) {
        self.component(i, self.component(i) * scalar)
        i = i + 1
      }
    }

    inline def divide(divisor: Double): Unit = {
      var i = 0
      while(i < dimension) {
        self.component(i, self.component(i) / divisor)
        i = i + 1
      }
    }

    inline def round(): Unit = {
      var i = 0
      while(i < dimension) {
        self.component(i, Math.round(self.component(i)).toDouble)
        i = i + 1
      }
    }

    inline def discretize(): Unit = round()

    inline def discretize(r: Double): Unit = {
      var i = 0
      while(i < dimension) {
        self.component(i, r * Math.round(self.component(i) / r).toDouble)
        i = i + 1
      }
    }

    inline def add(that: Euclidean): Unit = {
      sameDimensionOrThrowException(that)
      var i = 0
      while(i < dimension) {
        self.component(i, self.component(i) + that.component(i))
        i = i + 1
      }
    }

    inline def subtract(that: Euclidean): Unit = {
      sameDimensionOrThrowException(that)
      var i = 0
      while(i < dimension) {
        self.component(i, self.component(i) - that.component(i))
        i = i + 1
      }
    }

    def equals(that: Euclidean): Boolean = if (dimension == that.dimension) {
        var i:Int = 0
        while ( i < dimension && component(i) == that.component(i) ) i += 1
        i == dimension
    } else false

  }

}

case class ExtraDimensionalAccessException(e:Euclidean, ci: Int) extends Exception(
  s"Index: $ci exceeds dimensionality of Euclidean object${e.dimension}: $e"
)