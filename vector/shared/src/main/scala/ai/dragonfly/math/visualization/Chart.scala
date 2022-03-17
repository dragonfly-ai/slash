package ai.dragonfly.math.visualization

import scala.collection.mutable

import ai.dragonfly.math.*
import interval.*
import vector.Vector2
import example.Demonstrable

import Console.{BOLD, RESET}

object Chart extends Demonstrable {

  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {

    val lineChart:Chart = Chart("Test Line Chart", "X", "Y", `[]`(-10.0, 15.0), `[]`(-5.0, 3.0), 150, 60)

    var theta:Double = 0.0
    val increment:Double =  Math.PI / 10

    val lines:Array[(Vector2, Double)] = Array[(Vector2, Double)](
      (Vector2(-9.0, 0.0), -1.25),
      (Vector2(-7.0, 0.0), -1.0),
      (Vector2(-5.0, 0.0), -0.75),
      (Vector2(-3.0, 0.0), -0.5),
      (Vector2(-1.0, 0.0), -0.25),
      (Vector2(0.0, -1.0), 0.0),
      (Vector2(1.0, 0.0), 0.25),
      (Vector2(2.0, 0.0), 0.5),
      (Vector2(3.0, 0.0), 0.75),
      (Vector2(4.0, 0.0), 1.0),
      (Vector2(5.0, 0.0), 1.25)
    )

    for ((point:Vector2, slope:Double) <- lines) {
      val b: Double = (-point.x * slope) + point.y
      val title:String = s" Y = ${if (slope != 0.0) {
        "%4.2f".format(slope) + "X"
      } else ""} ${if (b == 0.0) "" else if (b > 0.0) s" + ${Math.abs(b)}" else s"- ${Math.abs(b)}"} "

      lineChart.line(
        point,
        slope,
        title
      )
    }

    sb.append(lineChart).append("\n")

    val scatterPlot:Chart = Chart("Test Scatter Plot", "X", "Y", `[]`(-10.0, 10.0), `[]`(-10.0, 10.0), 100, 100)
    val v2s:Array[Vector2] = new Array(50)
    for (i <- v2s.indices) v2s(i) = Vector2.random(20, 20).subtract(Vector2(10, 10))
    scatterPlot.scatter("Scatter 1", v2s:_*)
    sb.append(scatterPlot).append("\n")

    val scatterPlot1:Chart = Chart("Test Connected Scatter Plot", "X", "Y", `[]`(-10.0, 10.0), `[]`(-10.0, 10.0), 100, 100)
    for (i <- v2s.indices) v2s(i) = Vector2(
      (i * (20.0 / v2s.length)) - 10.0,
      (Math.random() * (20.0*(i+1.0) / v2s.length)) - (10.0*(i+1.0) / v2s.length)
    )
    scatterPlot1.connectedScatter("Connected Scatter 1", v2s:_*)
    sb.append(scatterPlot1).append("\n")

    val regressionPlot:Chart = Chart("Test Regression Plot", "Hours Studying", "Grade", `[]`(-10.0, 10.0), `[]`(-5.0, 5.0), 50, 25)

    val point:Vector2 = Vector2(1, 0)
    val slope:Double = 1.0 / 3.0
    val b:Double = (-point.x * slope) + point.y

    regressionPlot.line(point, slope, "Theory")

    var x:Double = regressionPlot.domain.min
    val step = (regressionPlot.domain.MAX - regressionPlot.domain.min) / v2s.length
    for (i <- v2s.indices) {
      v2s(i) = Vector2(x, (slope * x) + b + (4.0 * (Math.random() - 0.5)))
      println(v2s(i))
      x = x + step
    }
    regressionPlot.scatter("Practice", v2s:_*)
    sb.append(regressionPlot).append("\n")

    sb
  }

  override def name: String = "Chart"

}

object Glyph {
  def apply(id:Int):Glyph = {
    val color = 1 << (id % 3)
    if (id < 3) Glyph( "⠔", color, false )
    else Glyph( ConsoleImage.layerGlyphs((id / 3) - 1), color, true )
  }
}

case class Glyph(glyph: String, color:Int, overlay:Boolean) {
  override def toString: String = {
    if (overlay) s"${ConsoleImage.colorBytes(color)} $glyph $RESET"
    else s"${ConsoleImage.colorBytes(color)}$glyph $RESET"
  }
}

case class Chart(title:String, xLabel:String, yLabel:String, domain:Interval[Double], range:Interval[Double], width:Int, height:Int) {

  val cimg:ConsoleImage = ConsoleImage(width, height)

  val sX:Double = width / (domain.MAX - domain.min)
  val sY:Double = height / (range.MAX - range.min)

  // vertical axis ?
  if (domain.contains(0.0)) {
    val zeroX:Double = sX * Math.sqrt(squareInPlace(0.0 - domain.min))

    val start:Vector2 = Vector2(zeroX, 0.0)
    val end:Vector2 = Vector2(zeroX, height)

    ai.dragonfly.math.geometry.Line.discrete(start, end, (dX:Int, dY:Int) => {
      cimg.setGlyph(dX, (height - 1) - dY, "⃒", 8)
    })
  }

  // Horizontal Axis? "⃨⃛" "͞"
  if (range.contains(0.0)) {
    val zeroY:Double = sY * Math.sqrt(squareInPlace(0.0 - range.min))

    val start:Vector2 = Vector2(0.0, zeroY)
    val end:Vector2 = Vector2(width-3, zeroY)

    ai.dragonfly.math.geometry.Line.discrete(start, end, (dX:Int, dY:Int) => {
      cimg.setGlyph(dX, (cimg.height -1) - dY, "͞", 8)
    })
  }

  val leftPaddingWidth: Int = Math.max(Math.max(yLabel.length, range.MAX.toString.length), range.min.toString.length)

  private val legend:mutable.TreeMap[String, Glyph] = mutable.TreeMap[String, Glyph]()


  def glyphLineSegment(p1: Vector2, p2: Vector2, name:String, glyph:Glyph):Chart = {
    val xScale:Int = 2
    val yScale:Int = 5

//    val start:Vector2 = Vector2(0.0, (sY / yScale) * ((domain.min * slope + b) - range.min))
//    val end:Vector2 = Vector2((cimg.width - 1)/xScale, (sY / yScale) * ((domain.MAX * slope + b) - range.min))

    val start:Vector2 = Vector2(
      (sX / xScale) * (p1.x - domain.min),
      (sY / yScale) * (p1.y - range.min)
    )

    val end:Vector2 = Vector2(
      (sX / xScale) * (p2.x - domain.min),
      (sY / yScale) * (p2.y - range.min)
    )

    val hm:mutable.TreeMap[Int, Interval[Int]] = mutable.TreeMap[Int, Interval[Int]]()

    ai.dragonfly.math.geometry.Line.discrete(start, end, (dX:Int, dY:Int) => {
      val xi:Int = dX * xScale
      val yi:Int = (height - 1) - (dY * yScale)
      val interval = hm.getOrElseUpdate(yi, `[]`(xi, xi))
      hm.put(yi, `[]`(Math.min(interval.min, xi), Math.max(interval.MAX, xi)))
    })

    for ((yi:Int, xiv:Interval[Int]) <- hm) {
      val middle:Int = (xiv.min + xiv.MAX) / 2
      //cimg.setGlyph(Math.round(middle.toInt), yi, glyph.glyph, glyph.color)
      var s:Int = 1
      val step = 6
      while (xiv.MAX - (s*step) > middle) {
        cimg.setGlyph(xiv.min + (s*step), yi, glyph.glyph, glyph.color)
        cimg.setGlyph(xiv.MAX - (s*step), yi, glyph.glyph, glyph.color)
        s = s + 1
      }
      if (s < 2) cimg.setGlyph(middle, yi, glyph.glyph, glyph.color)
    }

    this
  }

  def glyphLine(point: Vector2, slope:Double, name:String, glyph:Glyph):Chart = {
    val b:Double = (-point.x * slope) + point.y

    val start:Vector2 = Vector2(domain.min, domain.min * slope + b)
    val end:Vector2 = Vector2(domain.MAX, domain.MAX * slope + b)

    glyphLineSegment(start, end, name, glyph)
  }

  private var maxItemNameLength:Int = 0

  def lineSegment(p1: Vector2, p2:Vector2, name:String):Chart = {
    val glyph = legend.getOrElseUpdate(name, Glyph(legend.size))
    maxItemNameLength = Math.max(maxItemNameLength, name.length + 2)

    if (glyph.overlay) {
      glyphLineSegment(p1, p2, name, glyph)
    } else {

      val start:Vector2 = Vector2(
        sX * (p1.x - domain.min),
        sY * (p1.y - range.min)
      )

      val end:Vector2 = Vector2(
        sX * (p2.x - domain.min),
        sY * (p2.y - range.min)
      )

      ai.dragonfly.math.geometry.Line.discrete(start, end, (dX:Int, dY:Int) => {
        cimg.setPixel(dX, (cimg.height - 1) - dY, glyph.color)
      })
      this
    }
  }

  def line(point: Vector2, slope:Double, name:String):Chart = {
    val glyph = legend.getOrElseUpdate(name, Glyph(legend.size))
    maxItemNameLength = Math.max(maxItemNameLength, name.length + 2)

    if (glyph.overlay) {
      glyphLine(point, slope, name, glyph)
    } else {
      val b:Double = (-point.x * slope) + point.y

      val start:Vector2 = Vector2(domain.min, domain.min * slope + b)
      val end:Vector2 = Vector2(domain.MAX, domain.MAX * slope + b)

      lineSegment(start, end, name)
    }
  }

  def scatter(name:String, points:Vector2*):Chart = {
    val glyph = legend.getOrElseUpdate(name, Glyph(legend.size))
    maxItemNameLength = Math.max(maxItemNameLength, name.length + 2)
    if (glyph.overlay) {
        for (p <- points) {
          cimg.setGlyph(
            (sX * (p.x - domain.min)).toInt,
            (cimg.height - 1) - (sY * (p.y - range.min)).toInt,
            glyph.glyph,
            glyph.color
          )
        }
      } else {
        for (p <- points) {
          cimg.setPixel(
            (sX * (p.x - domain.min)).toInt,
            (cimg.height - 1) - (sY * (p.y - range.min)).toInt,
            glyph.color
          )
        }
      }
    this
  }

  def connectedScatter(name:String, points:Vector2*):Chart = {
    var p = points.head
    var tail = points.tail

    while(tail.nonEmpty) {
      lineSegment(p, tail.head, name)

      p = tail.head
      tail = tail.tail
    }
    this
  }

  def padLeft(s:String)(using sb:StringBuilder):StringBuilder = {
    for (i <- 0 until leftPaddingWidth - s.length) sb.append(" ")
    sb.append(s)
  }

  def chartPadLeft(s:String)(using sb:StringBuilder):StringBuilder = {
    for (i <- 0 until ((width/2) - s.length) / 2) sb.append("⠀")
    sb.append(s)
  }

  def topBorder(using sb:StringBuilder):StringBuilder = {
    sb.append(" ⢀")
    for (i <- 0 until width/2) sb.append("⣀")
    sb.append("⡀")
  }

  def bottomBorder(using sb:StringBuilder):StringBuilder = {
    sb.append(" ⠈")
    for (i <- 0 until width/2) sb.append("⠉")
    sb.append("⠁")
  }

  override def toString: String = {
    given sb:StringBuilder = StringBuilder(RESET)
    val lines:Array[String] = cimg.lines
    padLeft("")
    chartPadLeft(title).append("\n")
    padLeft("")
    topBorder.append("\n")
    padLeft(range.MAX.toString).append(" ⢸").append(lines.head).append("⡇\n")
    val litr:Iterator[(String, Glyph)] = legend.iterator
    val footerLegend:String = if (legend.size > lines.length) {
      given lsb:StringBuilder = StringBuilder()
      padLeft("")
      var lineLength:Int = 0
      while(litr.hasNext) {
        val (itemName:String, itemGlyph:Glyph) = litr.next()
        val legendItem:String = s"$itemGlyph$itemName"
        if (lineLength + legendItem.length + 4 > width) {  // maxItemNameLength
          lsb.append("\n")
          padLeft("")
          lineLength = legendItem.length + 4
        } else {
          lineLength = lineLength + legendItem.length + 4
        }
        lsb.append(legendItem).append("  ")
      }
      lsb.toString()
    } else ""
    for (i <- 1 until lines.length - 1) {
      val l = if (i == lines.length / 2) yLabel else ""
      padLeft(l).append(" ⢸").append(lines(i)).append("⡇")
      if (litr.hasNext) {
        val (itemName, itemGlyph) = litr.next()
        sb.append(itemGlyph).append(itemName)
      }
      sb.append("\n")
    }
    padLeft(range.min.toString).append(" ⢸").append(lines.last).append("⡇\n")
    padLeft("")
    bottomBorder.append("\n")
    padLeft("").append(domain.min)
    chartPadLeft(xLabel)
    chartPadLeft(domain.MAX.toString)
      .append("\n")
      .append(footerLegend).append("\n")
      .toString()
  }

}
