package slash.matrix

import narr.*

trait MatFormat {
  def prefix(rows:Int, columns:Int, matValues:NArray[Double]): String

  def rowPrefix(rows:Int, columns:Int, matValues:NArray[Double]): String

  def delimiter(rows:Int, columns:Int, matValues:NArray[Double])(r: Int, c: Int): String

  def suffix(rows:Int, columns:Int, matValues:NArray[Double]): String

  def rowSuffix(rows:Int, columns:Int, matValues:NArray[Double]): String

  def format(d: Double): String = d.toString

  def render(
    rows:Int,
    columns:Int,
    matValues: NArray[Double],
    alignment: Function4[Int, Int, NArray[Double], MatFormat, NArray[NArray[String]]],
    sb: StringBuilder = new StringBuilder()
  ): StringBuilder = {

    val aligned:NArray[NArray[String]] = alignment(rows, columns, matValues, this)

    val dlm = delimiter(rows, columns, matValues)
    sb.append(prefix(rows, columns, matValues))

    var r = 0
    while (r < rows) {
      sb.append(rowPrefix(rows, columns, matValues))
      var c = 0
      while (c < columns) {
        sb.append(aligned(r)(c))
        sb.append(dlm(r, c))
        c = c + 1
      }
      sb.append(rowSuffix(rows, columns, matValues)).append("\n")
      r = r + 1
    }
    sb.append(suffix(rows, columns, matValues))
  }

  def columnMetrics(rows:Int, columns:Int, matValues:NArray[Double]): MatColumnMetrics = {
    val leftLength: NArray[Int] = NArray.fill[Int](columns)(0)
    val rightLength: NArray[Int] = NArray.fill[Int](columns)(0)
    val maxLength: NArray[Int] = NArray.fill[Int](columns)(0)

    var r = 0
    while (r < rows) {
      var c = 0
      while (c < columns) {
        val s = format(matValues(util.lindex(r, c, columns)))
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

  val UNALIGNED: Function4[Int, Int, NArray[Double], MatFormat, NArray[NArray[String]]] = (rows, columns, matValues: NArray[Double], fmt: MatFormat) => {
    NArray.tabulate[NArray[String]](rows)(
      (r: Int) => NArray.tabulate[String](columns)(
        (c: Int) => fmt.format(matValues(util.lindex(r, c, columns)))
      )
    )
  }

  val ALIGN_LEFT: Function4[Int, Int, NArray[Double], MatFormat, NArray[NArray[String]]] = (rows, columns, matValues: NArray[Double], fmt: MatFormat) => {
    val mcms:MatColumnMetrics = fmt.columnMetrics(rows, columns, matValues)
    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](rows)( (r: Int) => {
      NArray.tabulate[String](columns)((c: Int) => {
        val value = matValues(util.lindex(r, c, columns))
        var s = fmt.format(value)
        while (s.length < mcms.maxLength(c)) s = s + " "
        s
      })
    })
    out
  }

  val ALIGN_RIGHT: Function4[Int, Int, NArray[Double], MatFormat, NArray[NArray[String]]] = (rows, columns, matValues: NArray[Double], fmt: MatFormat) => {
    val mcms:MatColumnMetrics = fmt.columnMetrics(rows, columns, matValues)
    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](rows)( (r: Int) => {
      NArray.tabulate[String](columns)((c: Int) => {
        val value = matValues(util.lindex(r, c, columns))
        var s = fmt.format(value)
        while (s.length < mcms.maxLength(c)) s = " " + s
        s
      })
    })
    out
  }

  val ALIGN_CENTER: Function4[Int, Int, NArray[Double], MatFormat, NArray[NArray[String]]] = (rows, columns, matValues: NArray[Double], fmt: MatFormat) => {
    val mcms:MatColumnMetrics = fmt.columnMetrics(rows, columns, matValues)
    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](rows)( (r: Int) => {
      NArray.tabulate[String](columns)((c: Int) => {
        val value = matValues(util.lindex(r, c, columns))
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

  val ALIGN_ON_DECIMAL: Function4[Int, Int, NArray[Double], MatFormat, NArray[NArray[String]]] = (rows, columns, matValues: NArray[Double], fmt: MatFormat) => {
    val mcms:MatColumnMetrics = fmt.columnMetrics(rows, columns, matValues)
    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](rows)( (r: Int) => {
      NArray.tabulate[String](columns)((c: Int) => {
        val value = matValues(util.lindex(r, c, columns))
        val parts = fmt.format(value).split('.')
        while (parts(0).length < mcms.leftLength(c)) parts(0) = " " + parts(0)
        while (parts(1).length < mcms.rightLength(c)) parts(1) = parts(1) + " "
        parts(0) + "." + parts(1)
      })
    })
    out
  }

  //  val ALIGN_ON_MAGNITUDE: Function4[Int, Int, NArray[Double], MatFormat, NArray[NArray[String]]] = (rows, columns, matValues: NArray[Double], fmt: MatFormat) => {
  //    val mcms:MatColumnMetrics = fmt.columnMetrics(rows, columns, matValues)
  //    val out:NArray[NArray[String]] = NArray.tabulate[NArray[String]](rows)( (r: Int) => {
  //      NArray.tabulate[String](columns)((c: Int) => {
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
    override def prefix(rows:Int, columns:Int, matValues:NArray[Double]): String = s"Mat[${rows}, ${columns}](\n"

    override def delimiter(rows:Int, columns:Int, matValues:NArray[Double])(r: Int, c: Int): String = {
      if (c == columns - 1) {
        if (r == rows - 1) "" else ","
      } else ", "
    }

    override def suffix(rows:Int, columns:Int, matValues:NArray[Double]): String = ")\n"

    override def rowPrefix(rows:Int, columns:Int, matValues:NArray[Double]): String = "  "

    override def rowSuffix(rows:Int, columns:Int, matValues:NArray[Double]): String = ""
  }

  object TUPLE extends MatFormat {
    override def prefix(rows:Int, columns:Int, matValues:NArray[Double]): String = s"Mat(\n"

    override def delimiter(rows:Int, columns:Int, matValues:NArray[Double])(r: Int, c: Int): String = {
      if (c == columns - 1) {
        if (r == rows - 1) ")" else "),"
      } else ", "
    }

    override def suffix(rows:Int, columns:Int, matValues:NArray[Double]): String = ")\n"

    override def rowPrefix(rows:Int, columns:Int, matValues:NArray[Double]): String = "  ("

    override def rowSuffix(rows:Int, columns:Int, matValues:NArray[Double]): String = ""
  }

  object TEXTBOOK extends MatFormat {
    // Mat₍₃ₓ₅₎
    override def prefix(rows:Int, columns:Int, matValues:NArray[Double]): String = s"Mat${abase(rows)}ₓ${abase(columns)}\n"

    override def delimiter(rows:Int, columns:Int, matValues:NArray[Double])(r: Int, c: Int): String = " "

    override def suffix(rows:Int, columns:Int, matValues:NArray[Double]): String = "\n"

    override def rowPrefix(rows:Int, columns:Int, matValues:NArray[Double]): String = "│  "

    override def rowSuffix(rows:Int, columns:Int, matValues:NArray[Double]): String = " │\n"

    override def render(
      rows:Int,
      columns:Int,
      matValues: NArray[Double],
      alignment: Function4[Int, Int, NArray[Double], MatFormat, NArray[NArray[String]]],
      sb: StringBuilder = new StringBuilder()
    ): StringBuilder = {

      val aligned: NArray[NArray[String]] = alignment(rows, columns, matValues, this)

      val dlm = delimiter(rows, columns, matValues)
      sb.append(prefix(rows, columns, matValues))

      val firstRow: StringBuilder = new StringBuilder()
      var i:Int = 0
      while (i < columns) {
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

      sb.append(rowPrefix(rows, columns, matValues)).append(s"$firstRow").append(rowSuffix(rows, columns, matValues))
      var r = 1
      while (r < rows) {
        sb.append(rowPrefix(rows, columns, matValues))
        var c = 0
        while (c < columns) {
          sb.append(aligned(r)(c))
          sb.append(dlm(r, c))
          c = c + 1
        }
        sb.append(rowSuffix(rows, columns, matValues))
        r = r + 1
      }
      sb.append("└ ")

      i = 0
      while (i < firstRow.length) {
        sb.append(" ")
        i = i + 1
      }
      sb.append("  ┘")
      sb.append(suffix(rows, columns, matValues))
    }
  }

  object INDEXED extends MatFormat {
    override def prefix(rows:Int, columns:Int, matValues:NArray[Double]): String = s"Mat[${rows}x$columns]\n"

    override def suffix(rows:Int, columns:Int, matValues:NArray[Double]): String = ""

    override def delimiter(rows:Int, columns:Int, matValues:NArray[Double])(r: Int, c: Int): String = {
      val maxRowDigits = rows.toString.length
      val maxColDigits = columns.toString.length
      var rs = abase(r + 1)
      while (rs.length < maxRowDigits) rs = "₀" + rs
      var cs = abase(c + 1)
      while (cs.length < maxColDigits) cs = "₀" + cs
      s"$rs,$cs "
    }

    override def rowPrefix(rows:Int, columns:Int, matValues:NArray[Double]): String = "│  "

    override def rowSuffix(rows:Int, columns:Int, matValues:NArray[Double]): String = "│"
  }

  case class Delimited(delimeter: String) extends MatFormat {
    override def prefix(rows:Int, columns:Int, matValues:NArray[Double]): String = ""

    override def suffix(rows:Int, columns:Int, matValues:NArray[Double]): String = ""

    override def delimiter(rows:Int, columns:Int, matValues:NArray[Double])(r: Int, c: Int): String = {
      if (c == columns - 1) "" else s"$delimeter "
    }

    override def rowPrefix(rows:Int, columns:Int, matValues:NArray[Double]): String = ""

    override def rowSuffix(rows:Int, columns:Int, matValues:NArray[Double]): String = ""
  }

  lazy val CSV: MatFormat = Delimited(",")

  lazy val TSV: MatFormat = Delimited("\t")


  object ASCII extends MatFormat {
    override def prefix(rows:Int, columns:Int, matValues:NArray[Double]): String = ""

    override def delimiter(rows:Int, columns:Int, matValues:NArray[Double])(r: Int, c: Int): String = if (c == columns - 1) "" else ", "

    override def suffix(rows:Int, columns:Int, matValues:NArray[Double]): String = ""

    override def rowPrefix(rows:Int, columns:Int, matValues:NArray[Double]): String = "| "

    override def rowSuffix(rows:Int, columns:Int, matValues:NArray[Double]): String = " |"
  }

  def main(args: Array[String]): Unit = {
    println("Matrix Printing Demo")

    import slash.Random.*
    val r: scala.util.Random = defaultRandom

    val m = r.nextMatrix[10, 15](Short.MinValue.toDouble, Short.MaxValue.toDouble)
    //val m = r.nextMatrix[10, 10](Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.MxN)) = Double.MinPositiveValue
    m.values(r.nextInt(m.MxN)) = Math.random()
    m.values(r.nextInt(m.MxN)) = Math.random()
    m.values(r.nextInt(m.MxN)) = Math.random() / 9875379845.0
    m.values(r.nextInt(m.MxN)) = Math.random() / 9875379845.0
    m.values(r.nextInt(m.MxN)) = Math.random() / 9875379845.0
    m.values(r.nextInt(m.MxN)) = r.between(Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.MxN)) = r.between(Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.MxN)) = r.between(Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.MxN)) = r.between(Float.MinValue.toDouble, Float.MaxValue.toDouble)
    m.values(r.nextInt(m.MxN)) = Double.MaxValue



    val alignments:Array[(String, Function4[Int, Int, NArray[Double], MatFormat, NArray[NArray[String]]])] = {
      Array[(String, Function4[Int, Int, NArray[Double], MatFormat, NArray[NArray[String]]])](
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
