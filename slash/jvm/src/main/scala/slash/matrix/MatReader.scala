/*
 * Copyright 2025 dragonfly.ai
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
//> using dep ai.dragonfly::slash:0.3-42a6354-SNAPSHOT
package slash.matrix

import narr.NArray
import scala.io.Source
import java.net.URL
import java.nio.file.{Files, Paths, Path}
import java.nio.charset.StandardCharsets
import scala.collection.mutable.ArrayBuffer

object MatReader {
  /**
   * read Mat with explicit dimensions
   * @param resource filename String or URL
   */
  def readMatMxN[M <: Int, N <: Int](resource: String | URL)(using ValueOf[M], ValueOf[N]): Mat[M, N] = {
    val matrix = readMat(resource)
    val (rows, cols) = (matrix.rows, matrix.columns)
    val (r, c) = (valueOf[M], valueOf[N])
    require(r == rows && c == cols, s"expecting $r x $c but found $rows x $cols")
    matrix.asInstanceOf[Mat[M,N]]
  }

  /** Construct a matrix from a .csv file or URL.
   *
   * @param resource filename or URL.
   * @param delimiter optional delimiter String, default: ""
   * @param n number of rows to review for delimiter guess
   * @return Mat[M,N]
   * @throws IllegalArgumentException All rows must have the same length
   *
   * if @param delimiter is not specified, provide a guess based on 1st @param n rows.
   * non-numeric values replaced with NaN
   * rows with only NaN values are discarded
   */
  inline def readMat(inline resource: String | URL, delimiter: Char = '\u0000', n: Int = 20) = {
    def readLines: Iterator[String] = resource match {
    case dataFile: String =>
      Source.fromFile(dataFile).getLines()
    case url: URL =>
      Source.fromURL(url).getLines()
    }
    val lines = readLines.toSeq

    val delim: Char = if (delimiter != '\u0000') {
      delimiter
    } else {
      guessDelimiter( lines.take(n).mkString("\n"), resource)
    }

    val rowvalues: Seq[Seq[Double]] = for {
      (line, i) <- lines.zipWithIndex
      cols = parseLine(line, delim).map ((s: String) => string2double(s)).filter { !_.isNaN }
      if cols.nonEmpty
    } yield cols.toSeq

    val rows: Int = lines.size
    val cols: Int = rowvalues.map { _.size }.distinct match {
    case num :: Nil =>
      num.toInt
    case list =>
      throw new IllegalArgumentException(s"All rows must have the same length: ${list.mkString("|")}")
    }

    val matsize1: Int = rows * cols
    val v:NArray[Double] = new NArray[Double](matsize1)

    var i:Int = 0
    for (cols <- rowvalues) {
      for (col <- cols) {
        v(i) = col
        i += 1
      }
    }
    val mat = inline (rows, cols) match {
    case (r, c) =>
      new Mat[r.type, c.type](v)
    }
    mat
  }

  inline def string2double(s: String): Double = try { s.toDouble } catch { case _: Exception => Double.NaN }

  /*
  * Examine sample text to guess delimiter String.
  * Provides a reasonably fast guess, but can potentially fail.
  *
  * Premise: choose the most frequently occurring candidate.
  * priority if tied counts: comma, tab, pipe and semicolon.
  * Ambiguous result returns empty string, unless @param ignoreErrors true.
  *
  * @param sampleText lines examined to estimate 
  * @param src inserted in exception error messages.
  * @param ignoreErrors ambiguity throws exception
  * @throws sys.error on ambigous results.
  */
  def guessDelimiter(sampleText: String, src: String | URL, ignoreErrors: Boolean = true): Char = {
    var (tabs, commas, semis, pipes) = (0, 0, 0, 0)
    sampleText.toCharArray.foreach {
      case '\t' => tabs += 1
      case ','  => commas += 1
      case ';'  => semis += 1
      case '|'  => pipes += 1
      case _    =>
    }
    // in case of tie between commas and tabs, commas win (TODO: configurable)
    (commas, tabs, pipes, semis) match {
    case (cms, tbs, pps, sms) if cms >= tbs && cms >= pps && cms >= sms => ','
    case (cms, tbs, pps, sms) if tbs >= cms && tbs >= pps && tbs >= sms => '\t'
    case (cms, tbs, pps, sms) if pps >  cms && pps >  tbs && pps >  sms => '|'
    case (cms, tbs, pps, sms) if sms >  cms && sms >  tbs && sms >  pps => ';'
    case _ if ignoreErrors => '\u0000'

    case _ =>
      sys.error(
        s"ambiguous delimiter: tabs[$tabs], commas[$commas], semis[$semis], pipes[$pipes] for Source:\n[${src}]"
      )
    }
  }

  def parseLine(line: String, delimiter: Char = ',', quoteChar: Char = '"'): Iterator[String] = {
    new Iterator[String]:
      val charCount = line.length
      var i = 0
      var inQuotes = false
      var currentField = new StringBuilder
      var fields = ArrayBuffer[String]()
      
      private def nextField(): String =
        while i < charCount do
          line(i) match
            case `quoteChar` =>
              inQuotes = !inQuotes
              if inQuotes && i + 1 < charCount && line(i + 1) == quoteChar then
                currentField.append(quoteChar)
                i += 1

            case `delimiter` if !inQuotes =>
              val field = currentField.toString
              currentField = new StringBuilder
              i += 1
              return field

            case char =>
              currentField.append(char)

          i += 1
        val field = currentField.toString  
        currentField = new StringBuilder // so hasNext fails
        field

      def hasNext: Boolean =
        i < charCount || currentField.nonEmpty || inQuotes match {
          case true =>
            true
          case false =>
            false
        }

      def next(): String = {
        if (i >= charCount && currentField.isEmpty && !inQuotes) throw new NoSuchElementException("next on empty iterator")
        nextField()
      }
  }

  // create `filePath` with (rows x cols) of random doubles
  def createRandomMatFile(filename: String, rows: Int, cols: Int): Path = {
    val filePath = Paths.get(filename)
    val mat = Mat.random[5, 10](-1.0, 2.0)
    val content: String = mat.toString
    Files.write(filePath, content.getBytes(StandardCharsets.UTF_8))
  }
}
