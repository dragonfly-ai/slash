package ai.dragonfly.math.matrix

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.interval.Interval.*
import ai.dragonfly.math.matrix.ml.data.StaticSupervisedData
import ai.dragonfly.math.matrix.ml.supervized.regression.{LinearRegressionQR, LinearRegressionSVD, *}
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.vector.Vec.*
import ai.dragonfly.viz.cli.*
import ai.dragonfly.math.interval.*

object DemoLinearRegression extends Demonstration {

  override def demo(): Unit = {

    println("\n\nLinear Regression Tests: ")
    println("\nSynthetic Tests: ")
    val slrt: SyntheticLinearRegressionTest[100, 2] = SyntheticLinearRegressionTest[100, 2](Vec[2](2.0, 1.0), 2.0, 1.1)
    println(s"Generated Synthetic Test Data: $slrt")

    val interval: Interval[Double] = `[]`[Double](-1.0, 15.0)

    val xPlot: Chart = Chart("X Component", "p.x", "f(p)", slrt.trainingData.domainComponent(0), interval, 100, 40)
    val yPlot: Chart = Chart("Y Component", "p.y", "f(p)", slrt.trainingData.domainComponent(1), interval, 100, 40)

    val xY = (0 until slrt.trainingData.sampleSize).map((i: Int) => {
      val lv = slrt.trainingData.labeledExample(i); Vec[2](lv.vector(0), lv.y)
    })
    val yY = (0 until slrt.trainingData.sampleSize).map((i: Int) => {
      val lv = slrt.trainingData.labeledExample(i); Vec[2](lv.vector(1), lv.y)
    })

    xPlot.scatter(" (p.x, f(p))", xY: _*)
    yPlot.scatter(" (p.y, f(p))", yY: _*)

    println("\nTest LinearRegressionQR:\n")

    val syntProbLR: LinearRegressionProblem[100, 2] = LinearRegressionProblem[100, 2](slrt.trainingData)
    val slrQR = LinearRegressionQR[100, 2].train(syntProbLR)
    println(s"\tLinearRegressionQR.train(syntProbLR) => $slrQR\n")
    println(s"\tslrt.evaluate(slrQR) => ${slrt.evaluate(slrQR)}\n")

    val p = slrt.trainingData.sampleMean
    var yMean: Double = slrQR(p)

    val xSlopeQR = slrQR(p + Vec[2](1.0, 0.0)) - yMean
    val ySlopeQR = slrQR(p + Vec[2](0.0, 1.0)) - yMean

    xPlot.line(Vec[2](p(0), yMean), xSlopeQR, "QR (p.x, f'(p))")
    yPlot.line(Vec[2](p(1), yMean), ySlopeQR, "QR (p.y, f'(p))")

    println("\n\nTest LinearRegressionSVD:\n")
    val slrSVD = LinearRegressionSVD[100, 2].train(syntProbLR)
    println(s"\tLinearRegressionSVD.train(syntProbLR) => $slrSVD\n")
    println(s"\tslrt.evaluate(slrSVD) => ${slrt.evaluate(slrSVD)}\n")

    yMean = slrSVD(p)
    val xSlopeSVD: Double = slrSVD(p + Vec[2](1.0, 0.0)) - yMean
    val ySlopeSVD: Double = slrSVD(p + Vec[2](0.0, 1.0)) - yMean

    yMean / slrSVD.a.magnitude
    xPlot.line(Vec[2](p(0), yMean), xSlopeSVD, "SVD (p.x, f'(p))")
    yPlot.line(Vec[2](p(1), yMean), ySlopeSVD, "SVD (p.y, f'(p))")

    println(xPlot)
    println(yPlot)

    println("\nEmperical Tests:\n")
    val empericalTrainingData: StaticSupervisedData[100, 8] = new StaticSupervisedData[100, 8](EmpericalData.trainingData_01)
    val empericalTestData: StaticSupervisedData[100, 8] = new StaticSupervisedData[100, 8](EmpericalData.testData_01)
    val elrt: EmpiricalRegressionTest[100, 8] = EmpiricalRegressionTest[100, 8](empericalTrainingData, empericalTestData)
    val emProbLR: LinearRegressionProblem[100, 8] = LinearRegressionProblem[100, 8](empericalTrainingData)

    println(empericalTrainingData.sampleSize)

    println("\nTest LinearRegressionQR:\n")
    val elrQR = new LinearRegressionQR[100, 8].train(emProbLR)
    println(s"\tLinearRegressionQR.train(emProbLR) => $elrQR\n")
    println(s"\tslrt.evaluate(elrQR) => ${elrt.evaluate(elrQR)}\n")

    println("\n\nTest LinearRegressionSVD:\n")
    val elrSVD = new LinearRegressionSVD[100, 8].train(emProbLR)
    println(s"\tLinearRegressionSVD.train(emProbLR) => $elrSVD\n")
    println(s"\tslrt.evaluate(elrSVD) => ${elrt.evaluate(elrSVD)}\n")

  }

  override def name: String = "QR and SVD Linear Regression: "

}
