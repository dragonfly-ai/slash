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

package slash.matrix

import narr.*

trait MatFormat {
  def prefix(matValues:MatrixData): String

  def rowPrefix(matValues:MatrixData): String

  def delimiter(matValues:MatrixData)(r: Int, c: Int): String

  def suffix(matValues:MatrixData): String

  def rowSuffix(matValues:MatrixData): String

  def format(d: Double): String = d.toString

  def render(
    matValues:MatrixData,
    alignment: Function2[MatrixData, MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): StringBuilder = {

    val aligned:NArray[NArray[String]] = alignment(matValues, this)

    val dlm = delimiter(matValues)
    sb.append(prefix(matValues))

    var r = 0
    while (r < matValues.rowDimension) {
      sb.append(rowPrefix(matValues))
      var c = 0
      while (c < matValues.columnDimension) {
        sb.append(aligned(r)(c))
        sb.append(dlm(r, c))
        c = c + 1
      }
      sb.append(rowSuffix(matValues)).append("\n")
      r = r + 1
    }
    sb.append(suffix(matValues))
  }

  def columnMetrics(matValues:MatrixData): MatColumnMetrics = {
    val leftLength: NArray[Int] = NArray.fill[Int](matValues.columnDimension)(0)
    val rightLength: NArray[Int] = NArray.fill[Int](matValues.columnDimension)(0)
    val maxLength: NArray[Int] = NArray.fill[Int](matValues.columnDimension)(0)

    var r = 0
    while (r < matValues.rowDimension) {
      var c = 0
      while (c < matValues.columnDimension) {
        val s = format(matValues(r, c))
        val parts = s.split('.')

        leftLength(c) = Math.max(leftLength(c), parts(0).length)
        rightLength(c) = Math.max(rightLength(c), parts(1).length)
        maxLength(c) = Math.max(maxLength(c), s.length)

        c = c + 1
      }
      r = r + 1
    }
    MatColumnMetrics(leftLength, rightLength, maxLength)
  }
}

object MatFormat {

  import slash.unicode.*

  val UNALIGNED: Function2[MatrixData, MatFormat, NArray[NArray[String]]] = (matValues: MatrixData, fmt: MatFormat) => {
    NArray.tabulate[NArray[String]](matValues.rowDimension)(
      (r: Int) => NArray.tabulate[String](matValues.columnDimension)(
        (c: Int) => fmt.format(matValues(r, c))
      )
    )
  }

  val ALIGN_LEFT: Function2[MatrixData, MatFormat, NArray[NArray[String]]] = (matValues: MatrixData, fmt: MatFormat) => {
    val mcms:MatColumnMetrics = fmt.columnMetrics(matValues)
    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](matValues.rowDimension)( (r: Int) => {
      NArray.tabulate[String](matValues.columnDimension)((c: Int) => {
        val value = matValues(r, c)
        var s = fmt.format(value)
        while (s.length < mcms.maxLength(c)) s = s + " "
        s
      })
    })
    out
  }

  val ALIGN_RIGHT: Function2[MatrixData, MatFormat, NArray[NArray[String]]] = (matValues: MatrixData, fmt: MatFormat) => {
    val mcms:MatColumnMetrics = fmt.columnMetrics(matValues)
    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](matValues.rowDimension)( (r: Int) => {
      NArray.tabulate[String](matValues.columnDimension)((c: Int) => {
        val value = matValues(r, c)
        var s = fmt.format(value)
        while (s.length < mcms.maxLength(c)) s = " " + s
        s
      })
    })
    out
  }

  val ALIGN_CENTER: Function2[MatrixData, MatFormat, NArray[NArray[String]]] = (matValues: MatrixData, fmt: MatFormat) => {
    val mcms:MatColumnMetrics = fmt.columnMetrics(matValues)
    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](matValues.rowDimension)( (r: Int) => {
      NArray.tabulate[String](matValues.columnDimension)((c: Int) => {
        val value = matValues(r, c)
        var s = fmt.format(value)
        while (s.length < mcms.maxLength(c)) {
          s = " " + s
          if (s.length < mcms.maxLength(c)) s = s + " "
        }
        s
      })
    })
    out
  }

  val ALIGN_ON_DECIMAL: Function2[MatrixData, MatFormat, NArray[NArray[String]]] = (matValues: MatrixData, fmt: MatFormat) => {
    val mcms:MatColumnMetrics = fmt.columnMetrics(matValues)
    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](matValues.rowDimension)( (r: Int) => {
      NArray.tabulate[String](matValues.columnDimension)((c: Int) => {
        val value = matValues(r, c)
        val parts = fmt.format(value).split('.')
        while (parts(0).length < mcms.leftLength(c)) parts(0) = " " + parts(0)
        while (parts(1).length < mcms.rightLength(c)) parts(1) = parts(1) + " "
        parts(0) + "." + parts(1)
      })
    })
    out
  }

  //  val ALIGN_ON_MAGNITUDE: Function2[MatrixData, MatFormat, NArray[NArray[String]]] = (matValues: MatrixData, fmt: MatFormat) => {
  //    val mcms:MatColumnMetrics = fmt.columnMetrics(matValues)
  //    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](matValues.rowDimension)( (r: Int) => {
  //      NArray.tabulate[String](matValues.columnDimension)((c: Int) => {
  //        val value = m(r, c)
  //        var s = fmt.format(value)
  //        //        val xpnnt: Int = slash.native.getExponent(value)
  //        //        if (xpnnt > 0) {
  //        //          val parts = s.split('.')
  //        //          while (parts(0).length < mcms.leftLength(c)) parts(0) = " " + parts(0)
  //        //          while (parts(1).length < mcms.rightLength(c)) parts(1) = parts(1) + " "
  //        //          s = parts(0) + "." + parts(1)
  //        //        } else if (xpnnt < 0) {
  //        //          val parts = s.split('.')
  //        //          while (parts(0).length < mcms.leftLength(c)) parts(0) = " " + parts(0)
  //        //          while (parts(1).length < mcms.rightLength(c)) parts(1) = parts(1) + " "
  //        //          s = parts(0) + "." + parts(1)
  //        //        } else {
  //        val parts = s.split('.')
  //        while (parts(0).length < mcms.leftLength(c)) parts(0) = " " + parts(0)
  //        while (parts(1).length < mcms.rightLength(c)) parts(1) = parts(1) + " "
  //        s = parts(0) + "." + parts(1)
  //        //        }
  //        s
  //      })
  //    })
  //    out
  //  }

  object DEFAULT extends MatFormat {
    override def prefix(matValues:MatrixData): String = s"Mat[${matValues.rowDimension}, ${matValues.columnDimension}](\n"

    override def delimiter(matValues:MatrixData)(r: Int, c: Int): String = {
      if (c == matValues.columnDimension - 1) {
        if (r == matValues.rowDimension - 1) "" else ","
      } else ", "
    }

    override def suffix(matValues:MatrixData): String = ")\n"

    override def rowPrefix(matValues:MatrixData): String = "  "

    override def rowSuffix(matValues:MatrixData): String = ""
  }

  object TUPLE extends MatFormat {
    override def prefix(matValues:MatrixData): String = s"Mat(\n"

    override def delimiter(matValues:MatrixData)(r: Int, c: Int): String = {
      if (c == matValues.columnDimension - 1) {
        if (r == matValues.rowDimension - 1) ")" else "),"
      } else ", "
    }

    override def suffix(matValues:MatrixData): String = ")\n"

    override def rowPrefix(matValues:MatrixData): String = "  ("

    override def rowSuffix(matValues:MatrixData): String = ""
  }

  object TEXTBOOK extends MatFormat {
    // Mat₍₃ₓ₅₎
    override def prefix(matValues:MatrixData): String = s"Mat${abase(matValues.rowDimension)}ₓ${abase(matValues.columnDimension)}\n"

    override def delimiter(matValues:MatrixData)(r: Int, c: Int): String = " "

    override def suffix(matValues:MatrixData): String = "\n"

    override def rowPrefix(matValues:MatrixData): String = "│  "

    override def rowSuffix(matValues:MatrixData): String = " │\n"

    override def render(
      matValues: MatrixData,
      alignment: Function2[MatrixData, MatFormat, NArray[NArray[String]]],
      sb: StringBuilder = new StringBuilder()
    ): StringBuilder = {

      val aligned: NArray[NArray[String]] = alignment(matValues, this)

      val dlm = delimiter(matValues)
      sb.append(prefix(matValues))

      val firstRow: StringBuilder = new StringBuilder()
      var i:Int = 0
      while (i < matValues.columnDimension) {
        firstRow.append(aligned(0)(i))
        firstRow.append(dlm(0, i))
        i = i + 1
      }

      sb.append("┌ ")
      i = 0
      while (i < firstRow.length) {
        sb.append(" ")
        i = i + 1
      }
      sb.append("  ┐\n")

      sb.append(rowPrefix(matValues)).append(s"$firstRow").append(rowSuffix(matValues))
      var r = 1
      while (r < matValues.rowDimension) {
        sb.append(rowPrefix(matValues))
        var c = 0
        while (c < matValues.columnDimension) {
          sb.append(aligned(r)(c))
          sb.append(dlm(r, c))
          c = c + 1
        }
        sb.append(rowSuffix(matValues))
        r = r + 1
      }
      sb.append("└ ")

      i = 0
      while (i < firstRow.length) {
        sb.append(" ")
        i = i + 1
      }
      sb.append("  ┘")
      sb.append(suffix(matValues))
    }
  }

  object INDEXED extends MatFormat {
    override def prefix(matValues:MatrixData): String = s"Mat[${matValues.rowDimension}x${matValues.columnDimension}]\n"

    override def suffix(matValues:MatrixData): String = ""

    override def delimiter(matValues:MatrixData)(r: Int, c: Int): String = {
      val maxRowDigits = matValues.rowDimension.toString.length
      val maxColDigits = matValues.columnDimension.toString.length
      var rs = abase(r + 1)
      while (rs.length < maxRowDigits) rs = "₀" + rs
      var cs = abase(c + 1)
      while (cs.length < maxColDigits) cs = "₀" + cs
      s"$rs,$cs "
    }

    override def rowPrefix(matValues:MatrixData): String = "│  "

    override def rowSuffix(matValues:MatrixData): String = "│"
  }

  case class Delimited(delimeter: String) extends MatFormat {
    override def prefix(matValues:MatrixData): String = ""

    override def suffix(matValues:MatrixData): String = ""

    override def delimiter(matValues:MatrixData)(r: Int, c: Int): String = {
      if (c == matValues.columnDimension - 1) "" else s"$delimeter "
    }

    override def rowPrefix(matValues:MatrixData): String = ""

    override def rowSuffix(matValues:MatrixData): String = ""
  }

  lazy val CSV: MatFormat = Delimited(",")

  lazy val TSV: MatFormat = Delimited("\t")


  object ASCII extends MatFormat {
    override def prefix(matValues:MatrixData): String = ""

    override def delimiter(matValues:MatrixData)(r: Int, c: Int): String = if (c == matValues.columnDimension - 1) "" else ", "

    override def suffix(matValues:MatrixData): String = ""

    override def rowPrefix(matValues:MatrixData): String = "| "

    override def rowSuffix(matValues:MatrixData): String = " |"
  }

  def main(args: Array[String]): Unit = {
    println("Matrix Printing Demo")

    import slash.Random.*
    val r: scala.util.Random = defaultRandom

    val m = r.nextMatrix[10, 15](Short.MinValue.toDouble, Short.MaxValue.toDouble)
    //val m = r.nextMatrix[10, 10](Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.rowDimension), r.nextInt(m.columnDimension)) = Double.MinPositiveValue
    m.values(r.nextInt(m.rowDimension), r.nextInt(m.columnDimension)) = Math.random()
    m.values(r.nextInt(m.rowDimension), r.nextInt(m.columnDimension)) = Math.random()
    m.values(r.nextInt(m.rowDimension), r.nextInt(m.columnDimension)) = Math.random() / 9875379845.0
    m.values(r.nextInt(m.rowDimension), r.nextInt(m.columnDimension)) = Math.random() / 9875379845.0
    m.values(r.nextInt(m.rowDimension), r.nextInt(m.columnDimension)) = Math.random() / 9875379845.0
    m.values(r.nextInt(m.rowDimension), r.nextInt(m.columnDimension)) = r.between(Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.rowDimension), r.nextInt(m.columnDimension)) = r.between(Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.rowDimension), r.nextInt(m.columnDimension)) = r.between(Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.rowDimension), r.nextInt(m.columnDimension)) = r.between(Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.rowDimension), r.nextInt(m.columnDimension)) = Double.MaxValue

    val alignments:Array[(String, Function2[MatrixData, MatFormat, NArray[NArray[String]]])] = {
      Array[(String, Function2[MatrixData, MatFormat, NArray[NArray[String]]])](
        ("MatFormat.ALIGN_RIGHT", MatFormat.ALIGN_RIGHT),
        ("MatFormat.ALIGN_LEFT", MatFormat.ALIGN_LEFT),
        ("MatFormat.ALIGN_CENTER", MatFormat.ALIGN_CENTER),
        ("MatFormat.ALIGN_ON_DECIMAL", MatFormat.ALIGN_ON_DECIMAL)
      )
    }

    println("Unaligned CSV:")
    println(m.csv)
    println("Unaligned TSV:")
    println(m.tsv)

    for (a <- alignments) {
      println(s"\n/******** ${a._1} ********/\n")
      println("DEFAULT:")
      println(m.render(alignment = a._2))
      println("TUPLE:")
      println(m.render(TUPLE, a._2))
      println("TEXTBOOK:")
      println(m.render(TEXTBOOK, a._2))
      println("CSV:")
      println(m.csv(a._2))
      println("TSV:")
      println(m.tsv(a._2))
      println("ASCII:")
      println(m.render(ASCII, a._2))
      println("INDEXED:")
      println(m.render(INDEXED, a._2))
    }
  }
}

case class MatColumnMetrics(leftLength: NArray[Int], rightLength: NArray[Int], maxLength: NArray[Int])
