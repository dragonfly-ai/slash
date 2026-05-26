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

package slash.matrix.ml.data

import narr.*
import slash.*
import interval.*
import vector.runtime.{RTVec, *}
import stats.{LabeledRTVec, SimpleLabeledRTVector}
import stats.probability.distributions.{EstimatedGaussian, stream}
import matrix.*

import scala.language.{existentials, implicitConversions}

trait RTData {
  def sampleSize:Int
  def dimension:Int
  def X:RTMat
  //def asVectors:Set[Vector]
  def example(i:Int):RTVec
  def sampleMean:RTVec
  def sampleVariance:RTVec
  def sampleStandardDeviation:RTVec
  def domainComponent(i:Int):Interval[Double]
  def domainBias:RTVec = sampleMean

  def zScore(x: RTVec): RTVec = {
    val o = (x - sampleMean)
    o.pointwiseMultiply(sampleStandardDeviation.reciprocal)
    o
  }

  def fromZScore(x: RTVec): RTVec = {
    val o = x.copy
    o.pointwiseMultiply(sampleStandardDeviation)
    o += sampleMean
    o
  }
}

trait RTUnsupervisedData extends RTData {

}

object RTStaticUnsupervisedData {
  inline def apply(sampleSize:Int, dimension:Int, examples:NArray[RTVec]):RTStaticUnsupervisedData = {
    new RTStaticUnsupervisedData(sampleSize, dimension, examples)
  }
}

class RTStaticUnsupervisedData(val sampleSize:Int, val dimension:Int, examples:NArray[RTVec]) extends RTUnsupervisedData {

  dimensionCheck(sampleSize, examples.length)
  dimensionCheck(dimension, examples(0).dimension)

  private val Xar:NArray[RTVec] = NArray.ofSize[RTVec](sampleSize)

  // Compute sample point statistics and populate Xar and X
  val temp:(RTVec, RTVec, RTVec, NArray[Interval[Double]]) = {
    val sampleVectorStats: stream.StreamingRTVectorStats = new stream.StreamingRTVectorStats(dimension)

    var i:Int = 0
    while (i < sampleSize) {
      sampleVectorStats(examples(i))
      i += 1
    }

    val sampleMean:RTVec = sampleVectorStats.average()

    i = 0
    while (i < sampleSize) {
      Xar(i) = examples(i) - sampleMean
      i += 1
    }

    val intervals:NArray[Interval[Double]] = NArray.ofSize[Interval[Double]](dimension)
    i = 0
    while (i < dimension) {
      intervals(i) = `[]`(sampleVectorStats.minValues(i), sampleVectorStats.maxValues(i))
      i += 1
    }
    (sampleMean, sampleVectorStats.variance, sampleVectorStats.standardDeviation, intervals)
  }
  //(sampleMean:Vector, sampleVariance:Vector, sampleStandardDeviation:Vector, intervals:NArray[Interval[Double]]): Vector
  override val sampleMean:RTVec = temp._1
  override val sampleVariance:RTVec = temp._2
  override val sampleStandardDeviation:RTVec = temp._3
  val intervals:NArray[Interval[Double]] = temp._4

  override def domainComponent(i: Int):Interval[Double] = intervals(i)

  override val X: RTMat = RTMat(sampleSize, dimension, Xar)

  override def example(i: Int): RTVec = Xar(i) + sampleMean

}

trait RTSupervisedData extends RTData {
  def y: RTVec
  def Y: RTMat
  def labeledExample(i:Int):LabeledRTVec
  def labelStats:EstimatedGaussian
  def rangeBias:Double = labelStats.sampleMean
}

class RTStaticSupervisedData(val sampleSize:Int, val dimension:Int, labeledExamples:NArray[LabeledRTVec]) extends RTSupervisedData {

  dimensionCheck(sampleSize, labeledExamples.length)
  dimensionCheck(dimension, labeledExamples(0).vector.dimension)

  private val Xar:NArray[RTVec] = NArray.ofSize[RTVec](sampleSize)
  private val Yar:NArray[Double] = NArray.ofSize[Double](sampleSize)

  // Compute the average Vector
  val temp = {
    val labelStatsEstimator = stream.Gaussian()
    val sampleVectorStats:stream.StreamingRTVectorStats = new stream.StreamingRTVectorStats(dimension)

    var i:Int = 0
    while (i < sampleSize) {
      sampleVectorStats.apply(labeledExamples(i).vector)
      labelStatsEstimator.observe(labeledExamples(i).y)
      i += 1
    }

    val sampleMean:RTVec = sampleVectorStats.average()
    val labelStats:EstimatedGaussian = labelStatsEstimator.estimate

    i = 0; while (i < sampleSize) {
      Xar(i) = labeledExamples(i).vector - sampleMean
      Yar(i) = labeledExamples(i).y - labelStats.sampleMean
      i += 1
    }

    val intervals:NArray[Interval[Double]] = NArray.ofSize[Interval[Double]](dimension)
    i = 0; while (i < dimension) {
      intervals(i) = `[]`(sampleVectorStats.minValues(i), sampleVectorStats.maxValues(i))
      i += 1
    }
    (labelStats, sampleMean, sampleVectorStats.variance, sampleVectorStats.standardDeviation, intervals)
  }

  override val labelStats:EstimatedGaussian = temp._1
  override val sampleMean:RTVec = temp._2
  override val sampleVariance:RTVec = temp._3
  override val sampleStandardDeviation:RTVec = temp._4

  val intervals: NArray[Interval[Double]] = temp._5

  override val y: RTVec = RTVec(Yar)

  override val X: RTMat = RTMat(sampleSize, dimension, Xar)
  override val Y: RTMat = y.asColumnMatrix

  def example(i: Int): RTVec = Xar(i) + sampleMean

  def labeledExample(i: Int): LabeledRTVec = SimpleLabeledRTVector(Yar(i) + labelStats.sampleMean, example(i))

  def domainComponent(i: Int): Interval[Double] = intervals(i)

}