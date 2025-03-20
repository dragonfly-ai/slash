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
import slash.matrix.*
import slash.matrix.Mat.*
import scala.io.Source
import java.net.URL
import java.nio.file.{Files, Paths, Path}
import java.nio.charset.StandardCharsets

class MatReaderTest extends munit.FunSuite {
  def createRandomMatFile(filePath: String, rows: Int, cols: Int): Path = {
    val p = Paths.get(filePath)
    val mat = Mat.random[5, 10](-1.0, 2.0)
    val content: String = mat.toString
    Files.write(p, content.getBytes(StandardCharsets.UTF_8))
  }

  def dataUrl: URL = new URL("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv")

  val (testrows, testcols) = (23, 17)
  type M = testrows.type
  type N = testcols.type
  val randomTestMatrix = Mat.random[M,N]( -1.0, 2.0)

  val dataCsv: Path = {
    val testFile = Files.createTempFile("tmp", s"mat${testrows}x${testcols}.csv")
    System.err.println(s"testfile[$testFile]")
    Files.write(testFile, randomTestMatrix.toString.trim.getBytes(StandardCharsets.UTF_8))
  }

  override def afterAll(): Unit = {
    if (Files.exists(dataCsv) && !Files.deleteIfExists(dataCsv)) {
      System.err.println(s"unable to delete temp file ${dataCsv}")
    }
  }

  test("can create matrix from .csv file") {
    val filename = dataCsv.toString
    val mat1 = MatReader.readMatMxN[testrows.type, testcols.type](filename)
    assert(mat1.rows == testrows && mat1.columns == testcols)
  }
  test("can create matrix from .csv file without explicit dimensions") {
    val filename = dataCsv.toString
    val mat2 = MatReader.readMat(filename)
    assert(mat2.rows == testrows && mat2.columns == testcols)
  }

  test("can create matrix from data URL with or without explicit dimensions") {
    val (rows, cols) = (1600, 12)
    val mat3 = MatReader.readMat(dataUrl) // if URL data dimensions are unknown or varying
    assert(mat3.rows == rows && mat3.columns == cols)
    val mat4 = MatReader.readMatMxN[rows.type, cols.type](dataUrl)
    assert(mat3.strictEquals(mat4))
  }
}

