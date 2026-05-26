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
import slash.interval.*
import slash.Random.nextDoubles
import slash.squareInPlace

/**
 * Extremely unsafe!  Assumes all array arguments have uniform dimensions.
 */
package object util {

  export narr.Extensions.given

  inline def zeros(dimension: Int): NArray[Double] = new DoubleArray(dimension)

  inline def ones(dimension: Int): NArray[Double] = NArray.fill[Double](dimension)(1.0)

  inline def random(dimension: Int): NArray[Double] = random(dimension, 0.0, 1.0, slash.Random.defaultRandom)

  inline def random(dimension: Int, MAX: Double): NArray[Double] = random(dimension, 0.0, MAX, slash.Random.defaultRandom)

  inline def random(dimension: Int, min: Double, MAX: Double): NArray[Double] = random(dimension, min, MAX, slash.Random.defaultRandom)

  inline def random(
    dimension: Int,
    min: Double,
    MAX: Double,
    r: scala.util.Random
  ): NArray[Double] = r.nextDoubles(dimension, min, MAX)

  inline def random(
    dimension: Int,
    interval: Interval[Double],
    r: scala.util.Random
  ): NArray[Double] = NArray.tabulate[Double](dimension)(_ => interval.random(r))

  inline def fill(dimension: Int, d: Double): NArray[Double] = NArray.fill[Double](dimension)(d)

  inline def tabulate(dimension: Int, f: Int => Double): NArray[Double] = NArray.tabulate[Double](dimension)(f)

  def midpoint[N <: Int](v0: NArray[Double], v1: NArray[Double]): NArray[Double] = {
    val c = vectorCopyAndAdd(v0, v1)
    scaleInPlace(c, 0.5)
    c
  }

  def blend[N <: Int](alpha: Double, v0: NArray[Double], v1: NArray[Double]): NArray[Double] = {
    val c = scaled(v0, alpha)
    vectorCopyAndAdd(c, scaled(v1, (1.0 - alpha)))
    c
  }

  def mean[N <: Int](vs: NArray[Double]*): NArray[Double] = {
    val mean: NArray[Double] = copy(vs.head)
    val dimension: Int = mean.length
    var i: Int = 0
    for (v <- vs.tail) {
      i = 0
      while (i < dimension) {
        mean(i) = mean(i) + v(i)
        i = i + 1
      }
    }

    i = 0
    while (i < dimension) {
      mean(i) = mean(i) / dimension
      i = i + 1
    }
    mean
  }

  def mean[N <: Int](vArr: NArray[NArray[Double]]): NArray[Double] = {
    val mean: NArray[Double] = copy(vArr(0))
    val dimension: Int = mean.length
    var i:Int = 1
    var j:Int = 0
    while (i < vArr.length) {
      j = 0
      while (j < dimension) {
        mean(j) = mean(j) + vArr(i)(j)
        j = j + 1
      }
      i = i + 1
    }

    j = 0
    while (j < dimension) {
      mean(j) = mean(j) / dimension
      j = j + 1
    }
    mean
  }

  inline def copy(v: NArray[Double]): NArray[Double] = NArray.copy[Double](v)
  
  // clamp methods

  def clamp(a: NArray[Double], lt: Double, gt: Double): Unit = {
    if (gt < lt) throw Exception(s"Invoked Vec[${a.length}].clamp(lt = $lt, gt = $gt) but gt < lt.")
    var i = 0;
    while (i < a.length) {
      if (a(i) < lt) a(i) = lt
      else if (a(i) > gt) a(i) = gt
      i = i + 1
    }
  }

  inline def clamp(v: NArray[Double], i: Interval[Double]): Unit = clamp(v, i.set_min, i.set_MAX)

  inline def clamped(v: NArray[Double], lt: Double, gt: Double): NArray[Double] = {
    val o: NArray[Double] = copy(v)
    clamp(o, lt, gt)
    o
  }

  inline def clamped(v: NArray[Double], i: Interval[Double]): NArray[Double] = clamped(v, i.set_min, i.set_MAX)

  def min(v: NArray[Double], lt: Double): Unit = {
    var i = 0;
    while (i < v.length) {
      if (v(i) < lt) v(i) = lt
      i = i + 1
    }
  }

  def MAX(v: NArray[Double], gt: Double): Unit = {
    var i = 0;
    while (i < v.length) {
      if (v(i) > gt) v(i) = gt
      i = i + 1
    }
  }

  inline def clampedMin(v: NArray[Double], lt: Double): NArray[Double] = {
    val o: NArray[Double] = copy(v)
    min(o, lt)
    o
  }
  inline def clampedMAX(v: NArray[Double], gt: Double): NArray[Double] = {
    val o: NArray[Double] = copy(v)
    MAX(v, gt)
    o
  }


  def sum(v: NArray[Double]): Double = {
    var sum = 0.0
    var i = 0;
    while (i < v.length) {
      sum = sum + v(i)
      i = i + 1
    }
    sum
  }

  inline def mean(v: NArray[Double]): Double = sum(v) / v.length

  //It is assumed, that we consider a sample rather than a complete population
  def variance(v: NArray[Double]): Double = {
    // https://www.cuemath.com/sample-variance-formula/
    val mn = mean(v)
    v.map(i => squareInPlace(i - mn)).sum / (v.length - 1)
  }

  // It is assumed, that we consider a sample rather than a complete population
  def stdDev(v: NArray[Double]): Double = {
    // https://www.cuemath.com/data/standard-deviation/
    val mu = mean(v)
    val diffs_2 = v.map(num => squareInPlace(num - mu))
    Math.sqrt(diffs_2.sum / (v.length - 1))
  }

  def covariance(v0: NArray[Double], v1: NArray[Double]): Double = {
    val m0 = mean(v0)
    val m1 = mean(v1)
    var cv: Double = 0
    var i: Int = 0;
    while (i < v0.length) {
      cv += (v0(i) - m0) * (v1(i) - m1)
      i += 1
    }
    cv / (v0.length - 1)
  }

  def normSquared(v: NArray[Double]): Double = {
    var mag2 = 0.0
    var i = 0;
    while (i < v.length) {
      mag2 = mag2 + squareInPlace(v(i))
      i = i + 1
    }
    mag2
  }

  inline def norm(v: NArray[Double]): Double = Math.sqrt(normSquared(v))

  inline def magnitude(v: NArray[Double]): Double = norm(v)

  inline def magnitudeSquared(v: NArray[Double]): Double = normSquared(v)

  def euclideanDistanceSquaredBetween(v0: NArray[Double], v1: NArray[Double]): Double = {
    var distance = 0.0
    var i = 0;
    while (i < v0.length) {
      distance = distance + squareInPlace(v0(i) - v1(i))
      i = i + 1
    }
    distance
  }

  inline def euclideanDistanceBetween(v0: NArray[Double], v1: NArray[Double]): Double = Math.sqrt(euclideanDistanceSquaredBetween(v0, v1))

  inline def scalarCopyAndAdd(v: NArray[Double], scalar: Double): NArray[Double] = {
    val c = copy(v)
    scalarAddInPlace(c, scalar)
    c
  }

  inline def scalarAddInPlace(v: NArray[Double], scalar: Double): Unit = {
    var i = 0;
    while (i < v.length) {
      v(i) = v(i) + scalar
      i = i + 1
    }
  }

  inline def vectorCopyAndAdd(v0: NArray[Double], v1: NArray[Double]): NArray[Double] = {
    val c = copy(v0)
    vectorAddInPlace(c, v1)
    c
  }

//  inline def +=(v0: NArray[Double]): Unit = add(v0)

  def vectorAddInPlace(v0: NArray[Double], v1: NArray[Double]): Unit = {
    var i = 0;
    while (i < v0.length) {
      v0(i) = v0(i) + v1(i)
      i = i + 1
    }
  }

  //inline def unary_- : NArray[Double] = thisVector * (-1.0)

  def scalarCopyAndSubtract(v: NArray[Double], scalar: Double): NArray[Double] = {
    val c = copy(v)
    scalarSubtractInPlace(c, scalar)
    c
  }

  def scalarSubtractInPlace(v: NArray[Double], scalar: Double): Unit = {
    var i = 0;
    while (i < v.length) {
      v(i) = v(i) - scalar
      i = i + 1
    }
  }

  def vectorCopyAndSubtract(v0: NArray[Double], v1: NArray[Double]): NArray[Double] = {
    val c = copy(v0)
    vectorSubtractInPlace(c, v1)
    c
  }

  //inline def -=(v0: NArray[Double]): Unit = subtract(v0)

  def vectorSubtractInPlace(v0: NArray[Double], v1: NArray[Double]): Unit = {
    var i = 0;
    while (i < v0.length) {
      v0(i) = v0(i) - v1(i)
      i = i + 1
    }
  }

  def dot(v0: NArray[Double], v1: NArray[Double]): Double = {
    var product = 0.0
    var i = 0;
    while (i < v0.length) {
      product = product + v0(i) * v1(i)
      i = i + 1
    }
    product
  }

//  inline def *(scalar: Double): NArray[Double] = scaled(scalar)
//  inline def *=(scalar: Double): Unit = scale(scalar)

  def scaleInPlace(v: NArray[Double], scalar: Double): Unit = {
    var i = 0;
    while (i < v.length) {
      v(i) = v(i) * scalar
      i = i + 1
    }
  }

  def scaled(v: NArray[Double], scalar: Double): NArray[Double] = {
    val c = copy(v)
    scaleInPlace(c, scalar)
    c
  }

//  inline def /(divisor: Double): NArray[Double] = divided(divisor)
//  inline def /=(divisor: Double): Unit = divide(divisor)

  inline def divideInPlace(v: NArray[Double], divisor: Double): Unit = scaleInPlace(v, 1.0 / divisor)

  def divided(v: NArray[Double], divisor: Double): NArray[Double] = {
    val c = copy(v)
    scaleInPlace(c, 1.0 / divisor)
    c
  }

  def reciprocate(v: NArray[Double]): Unit = {
    var i = 0;
    while (i < v.length) {
      v(i) = 1.0 / v(i)
      i = i + 1
    }
  }

  def reciprocal(v: NArray[Double]): NArray[Double] = {
    val c: NArray[Double] = copy(v)
    reciprocate(c)
    c
  }

  def pointwiseMultiply(v0: NArray[Double], v1: NArray[Double]): Unit = {
    var i = 0;
    while (i < v0.length) {
      v0(i) = v0(i) * v1(i)
      i = i + 1
    }
  }

  def pointwiseMultiplied(v0: NArray[Double], v1: NArray[Double]): NArray[Double] = {
    val c: NArray[Double] = copy(v0)
    pointwiseMultiply(v0, v1)
    c
  }

  def kronecker(thisVector: NArray[Double], thatVector: NArray[Double]): NArray[Double] = {
    val o: NArray[Double] = zeros(thisVector.length * thatVector.length)
    var i = 0
    while (i < thisVector.length) {
      var j = 0
      while (j < thatVector.length) {
        o(i * thatVector.length + j) = thisVector(i) * thatVector(j)
        j = j + 1
      }
      i = i + 1
    }
    o
  }

  def round(v: NArray[Double]): Unit = {
    var i = 0;
    while (i < v.length) {
      v(i) = Math.round(v(i)).toDouble
      i = i + 1
    }
  }

  inline def rounded(v: NArray[Double]): NArray[Double] = {
    val c: NArray[Double] = copy(v)
    round(c)
    c
  }

  inline def discretize(v: NArray[Double]): Unit = round(v)

  def discretize(v: NArray[Double], r: Double): Unit = {
    var i = 0;
    while (i < v.length) {
      v(i) = r * Math.round(v(i) / r).toDouble
      i = i + 1
    }
  }

  def discritized(v: NArray[Double]): NArray[Double] = {
    val c: NArray[Double] = copy(v)
    round(c)
    c
  }

  def discritized(v: NArray[Double], r: Double): NArray[Double] = {
    val c: NArray[Double] = copy(v)
    discretize(c, r)
    c
  }

  inline def normalize(v: NArray[Double]): Unit = {
    val n: Double = norm(v)
    if (n > 0.0) divideInPlace(v, n)
    else throw slash.exceptions.VectorNormalizationException(v.toString())
    //else throw slash.exceptions.VectorNormalizationException(render(v).toString())
  }

  def normalized(v: NArray[Double]): NArray[Double] = {
    val c: NArray[Double] = copy(v)
    normalize(c)
    c
  }

//  // ₂⃗ ²↗ ↗²
//  def show(v: NArray[Double]): String = v.length match {
//    case 2 => s"《²↗〉${v(0)}ᵢ ${v(1)}ⱼ〉"
//    case 3 => s"《³↗〉${v(0)}ᵢ ${v(1)}ⱼ ${v(2)}ₖ〉"
//    case 4 => s"《⁴↗〉${v(0)}ᵢ ${v(1)}ⱼ ${v(2)}ₖ ${v(3)}ₗ〉"
//    case _ => render(v).toString()
//  }
//
//  def render(v: NArray[Double], format: VecFormat = VecFormat.Default, sb: StringBuilder = new StringBuilder()): StringBuilder = {
//    import format.*
//    sb.append(prefix(v))
//    val end: Int = v.length - 1
//    var i = 0;
//    while (i < end) {
//      sb.append(numberFormatter(v(i)))
//        .append(delimiter(i))
//      i = i + 1
//    }
//    sb.append(numberFormatter(v(v.length - 1)))
//      .append(suffix(v))
//  }
//
//  def csv: String = csv(new StringBuilder()).toString
//  def csv(sb: StringBuilder = new StringBuilder()): String = render(VecFormat.CSV, sb).toString
//  def tsv: String = tsv(new StringBuilder()).toString
//  def tsv(sb: StringBuilder = new StringBuilder()): String = render(VecFormat.TSV, sb).toString
//
//  extension (d: Double) {
//    def *[N <: Int](v: NArray[Double]): NArray[Double] = v * d
//  }

}
