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

package slash.matrixf

import scala.collection.mutable.ArrayBuffer

object Util {

//  /** Construct a matrix from delimited String rows.
//    * @param lines
//    *   matrix String rows of delimited columns
//    * @param delimiter
//    *   optional delimiter Char
//    * @param n
//    *   number of rows to review for delimiter guess
//    * @return
//    *   MatF[M,N]
//    * @throws IllegalArgumentException
//    *   All rows must have the same length
//    */
//  inline def fromString(
//      inline content: String,
//      delimiter: Char = '\u0000',
//      n: Int = 20
//  ) = {
//    val lines = content.trim.linesIterator.to(LazyList)
//    val delim: Char = if (delimiter != '\u0000') {
//      delimiter
//    } else {
//      guessDelimiter(lines.take(n).mkString("\n"))
//    }
//
//    val values = for {
//      (line, i) <- lines.zipWithIndex
//      cols = parseLine(line, delim).map(string2float(_)).filter { !_.isNaN }
//      if cols.nonEmpty
//    } yield cols.toSeq
//
//    val rows: Int = lines.size
//    val cols: Int = {
//      values.map { _.size }.distinct.toList match {
//      case num :: Nil =>
//        num
//      case list =>
//        throw new IllegalArgumentException(
//          s"All rows must have the same length: ${list.mkString("|")}"
//        )
//      }
//    }
//
//    val matsize1: Int = rows * cols
//    val v: NArray[Float] = new NArray[Float](matsize1)
//
//    var i: Int = 0
//    for (cols <- values) {
//      for (col <- cols) {
//        v(i) = col
//        i += 1
//      }
//    }
//    inline (rows, cols) match {
//      case (r, c) =>
//        new MatF[r.type, c.type](v)
//    }
//  }

  inline def string2float(s: String): Double = try { s.toFloat }
  catch { case _: Exception => Double.NaN }

  /**
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
  def guessDelimiter(
      sampleText: String,
      ignoreErrors: Boolean = true
  ): Char = {
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
      case (cms, tbs, pps, sms) if tbs >= cms && tbs >= pps && tbs >= sms =>
        '\t'
      case (cms, tbs, pps, sms) if pps > cms && pps > tbs && pps > sms => '|'
      case (cms, tbs, pps, sms) if sms > cms && sms > tbs && sms > pps => ';'
      case _ if ignoreErrors => '\u0000'

      case _ =>
        sys.error(
          s"ambiguous delimiter: tabs[$tabs], commas[$commas], semis[$semis], pipes[$pipes]"
        )
    }
  }

  /**
   * Examine sample text to guess delimiter String.
   * @param line row of delimited column values
   * @param delimiter delimiter Char
   */
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
        if (i >= charCount && currentField.isEmpty && !inQuotes)
          throw new NoSuchElementException("next on empty iterator")
        nextField()
      }
  }
}
