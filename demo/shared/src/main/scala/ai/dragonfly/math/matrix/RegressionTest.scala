package ai.dragonfly.math.matrix

import ai.dragonfly.math.*
import ai.dragonfly.math.Random.*
import ai.dragonfly.math.matrix.ml.data.*
import ai.dragonfly.math.matrix.ml.supervized.regression.*
import ai.dragonfly.math.stats.*
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.vector.Vec.*
import narr.*

trait LinearRegressionTest[M <: Int, N <: Int] {
  def trainingData:SupervisedData[M, N]
  def testData:SupervisedData[M, N]
  def evaluate(model: LinearRegressionModel[N]):LinearRegressionTestScore
}

case class SyntheticLinearRegressionTest[M <: Int, N <: Int](trueCoefficients: Vec[N], bias: Double, noise:Double = 1.0)(using ValueOf[M], ValueOf[N]) extends LinearRegressionTest[M, N] {
  val sampleSize:Int = valueOf[M]
  val maxNorm:Double = trueCoefficients.dimension * trueCoefficients.magnitude //Math.min(2.0 * dimension, sampleSize)

  var syntheticError: Double = 0.0

  override val trainingData:SupervisedData[M, N] = {
    val td: NArray[LabeledVec[N]] = new NArray[LabeledVec[N]](sampleSize)

    var i:Int = 0; while (i < td.length) {
      val xi: Vec[N] = defaultRandom.nextVec[N](maxNorm)
      val yi: Double = f(xi)

      val yi_noisy = yi + (noise * (defaultRandom.between(-0.5, 0.5)))

      td(i) = SimpleLabeledVector[N](yi_noisy, xi)
      val err = yi_noisy - yi

      syntheticError = syntheticError + err * err
      i += 1
    }
    new StaticSupervisedData[M, N](td)
  }

  syntheticError = Math.sqrt(syntheticError / trainingData.sampleSize)

  private def f(xi:Vec[N]):Double = (xi dot trueCoefficients) + bias

  override def evaluate(model: LinearRegressionModel[N]):LinearRegressionTestScore = {
    var observedError:Double = 0.0
    var i:Int = 0; while (i < testData.sampleSize) {
      val lv = testData.labeledExample(i)
      val err = model(lv.x) - f(lv.x)
//      println(s"\ty = ${f(lv.x)} y' = ${model(lv.x)} error = $err : $lv")
      observedError = observedError + (err * err)
      i += 1
    }
    observedError = Math.sqrt( observedError / testData.sampleSize )
    LinearRegressionTestScore(model.standardError, observedError)
  }

  override def testData: SupervisedData[M, N] = trainingData
}


case class EmpiricalRegressionTest[M <: Int, N <: Int](override val trainingData:SupervisedData[M, N], override val testData:SupervisedData[M, N]) extends LinearRegressionTest[M, N] {
  override def evaluate(model: LinearRegressionModel[N]):LinearRegressionTestScore = {
    var observedError:Double = 0.0
    var i:Int = 0; while (i < testData.sampleSize) {
      val lv = testData.labeledExample(i)
      val err = model(lv.x) - lv.y
//      println(s"\ty = ${lv.y} y' = ${model(lv.x)} error = $err : $lv")
      observedError = observedError + (err * err)
      i += 1
    }
    observedError = Math.sqrt(observedError / testData.sampleSize)
    LinearRegressionTestScore(model.standardError, observedError)
  }
}

case class LinearRegressionTestScore(standardError:Double, testError:Double) {}
