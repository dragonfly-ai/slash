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

import narr.NArray
import slash.matrix.ml.data.StaticSupervisedData
import slash.matrix.ml.supervized.regression.*
import slash.squareInPlace
import slash.stats.{LabeledVec, SimpleLabeledVector}
import slash.vector.*

class LinearRegressionTest extends munit.FunSuite {

  val data = new StaticSupervisedData[6,3](
    NArray[LabeledVec[3]](
      SimpleLabeledVector[3](3.0 + Math.random() / 1000.0, Vec[3](1.0, 2.0, 3.0) + Vec.random[3](0.001)),
      SimpleLabeledVector[3](3.0 - Math.random() / 1000.0, Vec[3](1.0, 2.0, 3.0) + Vec.random[3](0.001)),
      SimpleLabeledVector[3](6.0 + Math.random() / 1000.0, Vec[3](2.0, 4.0, 6.0) + Vec.random[3](0.001)),
      SimpleLabeledVector[3](6.0 - Math.random() / 1000.0, Vec[3](2.0, 4.0, 6.0) + Vec.random[3](0.001)),
      SimpleLabeledVector[3](9.0 + Math.random() / 1000.0, Vec[3](3.0, 6.0, 9.0) + Vec.random[3](0.001)),
      SimpleLabeledVector[3](9.0 - Math.random() / 1000.0, Vec[3](3.0, 6.0, 9.0) + Vec.random[3](0.001)),
    )
  )

//  for (i <- 0 until 6) println(data.labeledExample(i))

  test("LinearRegressionQR") {

    val lrp:LinearRegressionProblem[6,3] = LinearRegressionProblem[6,3](data)
    val lr:LinearRegressionQR[6,3] = new LinearRegressionQR[6,3]
    val model:LinearRegressionModel[3] = lr.train(lrp)
//    println(model(Vec[3](1.0, 2.0, 3.0)))
    assert(Math.sqrt(squareInPlace(model(Vec[3](1.0, 2.0, 3.0)) - 3.0)) < 0.002)
  }

  test("LinearRegressionSV") {

    val lrp: LinearRegressionProblem[6,3] = LinearRegressionProblem[6,3](data)
    val lr: LinearRegressionSVD[6,3] = new LinearRegressionSVD[6,3]
    val model: LinearRegressionModel[3] = lr.train(lrp)
//    println(model(Vec[3](1.0, 2.0, 3.0)))
    assert(Math.sqrt(squareInPlace(model(Vec[3](1.0, 2.0, 3.0)) - 3.0)) < 0.002)
  }
}
