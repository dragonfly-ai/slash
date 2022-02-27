package ai.dragonfly.math.stats

import ai.dragonfly.math.stats.probability.distributions.{ProbabilityDistribution, Sampleable}
import ai.dragonfly.math.examples.Demonstrable

import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag

trait UnivariateHistogram[DOMAIN](using `#`: Numeric[DOMAIN] , tag: ClassTag[DOMAIN]) {
  import `#`._

  val size: Int // number of bins
  val min: DOMAIN
  val MAX: DOMAIN
  def apply(x: DOMAIN, weight: Double = 1.0): UnivariateHistogram[DOMAIN]
  def mass: Double
  def index(x:DOMAIN):Int
  def bINTerpolator(bINdex:Int, alpha:Double):DOMAIN
  def binMass(bINdex:Int):Double
  val bucketWidth: Double =  (MAX - min).toDouble / size.toDouble

  protected val integerDigits: Int = Math.log10(MAX.toDouble).toInt + 1
  protected val fractionDigits: Int = Math.log10( if (bucketWidth < 1.0) (1.0 / bucketWidth) else (100.0 / bucketWidth)).toInt + 1

  protected val binLabelFormat:String = s"%${integerDigits + fractionDigits + 1}.${fractionDigits}f"

  protected def pad(d:Double):String = {
    val padded:String = binLabelFormat.format(Math.abs(d))
    if (d < 0.0) {
      var s = "-" + padded.trim
      while (s.length <= padded.length) {
        s = " " + s
      }
      s
    } else {
      " " + padded
    }
  }

  def binLabel(bINdex:Int):String = {
    var floor = min.toDouble + (bINdex * bucketWidth)
    var ceiling = floor + bucketWidth
    val lastBracket = if (bINdex >= size - 1) "]" else ")"
    s"[${pad(floor)},${pad(ceiling)} $lastBracket"
  }

  val theme: Array[String] = Array[String](
    "🌑", "🌒", "🌓", "🌔", "🌕" // "🌑", "🌒", "🌓", "🌔", "🌕", "🌖", "🌗", "🌘", "🌑"
  )

  override def toString: String = {
    val capBlocks: Array[String] = Array[String](" ", "▏", "▎", "▍", "▌", "▋", "▊", "▉", "█" )  //

    var maxBinMass:Double = Double.MinValue
    for (bIndex <- index(min) to index(MAX)) {
      maxBinMass = Math.max(maxBinMass, binMass((bIndex)) / mass)
    }

    val scale:Double = 100.0 / maxBinMass

    val sb = new mutable.StringBuilder("Histogram:\n")

    var cumulative = 0.0
    for (bIndex <- index(min) to index(MAX)) {

      var bnms: Double = binMass(bIndex)
      cumulative = cumulative + bnms
      if (cumulative > 0.0 && (cumulative < mass || bnms > 0.0)) {
        bnms = bnms / mass

        sb.append(binLabel(bIndex)).append(" ")

        val glyph = theme(((cumulative / mass) * (theme.length - 1)).toInt)
        sb.append(glyph).append(" ")

        if (bnms > Double.MinPositiveValue) {
          val m = scale * bnms
          var wholeCount = m / capBlocks.length
          val `1/8`: Double = 1.0 / 8.0
          // base
          sb.append(
            if (wholeCount >= `1/8`) {
              wholeCount = wholeCount - `1/8`
              "▕"
            } else {
              if (wholeCount > `1/8` / 2.0) {
                wholeCount = wholeCount - `1/8` / 2.0
                "⢸"
              } else if (m > `1/8` / 10.0) {
                wholeCount = wholeCount - `1/8` / 10.0
//                "︙"
//              } else if (m > `1/8` / 16.0) {
//                wholeCount = wholeCount - `1/8` / 16.0
                "⠰"
              } else if (m > `1/8` / 100.0) {
                wholeCount = wholeCount - `1/8` / 100.0
                "⠐"
              } else ""
            }
          )
          // bar
          while (wholeCount > 1.0) {
            sb.append("█") // ░ ▒ ▓
            wholeCount = wholeCount - 1.0
          }
          // cap
          sb.append(capBlocks((wholeCount * capBlocks.length).toInt))
          if (bnms > Double.MinPositiveValue) sb.append(s"   ∝ $bnms")
        }
        sb.append("\n")
      }
    }
    sb.toString()
  }

}

class DenseHistogramOfDiscreteDistribution(override val size: Int, override val min: Long, override val MAX: Long) extends UnivariateHistogram[Long] {

  val hist: Array[Double] = Array.fill(size)(0.0)

  private var totalMass: Double = 0.0

  def apply(observation: Long, weight: Double): DenseHistogramOfDiscreteDistribution = {
    val bindex = index(observation)

    hist(bindex) = hist(bindex) + weight
    totalMass = totalMass + weight

    this
  }

  override def mass: Double = totalMass

  def binMass(bINdex: Int): Double = hist(bINdex)

  override def index(x: Long): Int = Math.min(((x - min).toDouble/bucketWidth).toInt, hist.length - 1)

  override def bINTerpolator(bINdex: Int, alpha: Double): Long = {
    ((alpha * (min + (bINdex * bucketWidth))) + ((1.0 - alpha) * (min + ((bINdex + 1) * bucketWidth)))).toLong
  }

}

class DenseHistogramOfContinuousDistribution(override val size: Int, override val min: Double, override val MAX: Double) extends UnivariateHistogram[Double] {

  val hist: Array[Double] = Array.fill(size)(0.0)

  private var totalMass: Double = 0.0

  def apply(observation: Double, weight: Double): DenseHistogramOfContinuousDistribution = {
    val bindex = index(observation)

    hist(bindex) = hist(bindex) + weight
    totalMass = totalMass + weight

    this
  }

  override def mass: Double = totalMass

  override def binMass(bINdex: Int): Double = hist(bINdex)

  override def index(x: Double): Int = Math.min(Math.floor((x - min)/bucketWidth).toInt, hist.length - 1)

  override def bINTerpolator(bINdex: Int, alpha: Double): Double = {
    (alpha * (min + (bINdex * bucketWidth))) + ((1.0 - alpha) * (min + ((bINdex + 1) * bucketWidth)))
  }

}
//
//class SparseDiscreteHistogram(override val size: Int, override val min: Double, override val MAX: Double) extends UnivariateHistogram[Double] {
//
//  override val bucketWidth: Double = (MAX - min) / size
//
//  val hist: mutable.HashMap[Int, Double] = mutable.HashMap[Int, Double]()
//  private var totalMass = 0.0
//  def mass:Double = totalMass
//  def apply(observation: Double, weight: Double = 1.0): SparseDiscreteHistogram = {
//    val bindex = index(observation)
//
//    hist.get(bindex) match {
//      case Some(frequency: Double) => hist.put(bindex, frequency + weight)
//      case None => hist.put(bindex, weight)
//    }
//    totalMass = totalMass + weight
//    this
//  }
//
//  override protected def binMass(bindex: Int): Double = hist.getOrElse[Double](bindex, 0.0)
//}
//
//class SparseOrderedDiscreteHistogram(override val size: Int, override val min: Double, override val MAX: Double) extends UnivariateHistogram[Double] {
//
//  override val bucketWidth: Double = (MAX - min) / size
//
//  val hist: mutable.TreeMap[Int, Double] = mutable.TreeMap[Int, Double]()
//  private var totalMass = 0.0
//  override def getTotalMass: Double = totalMass
//  def apply(observation: Double, weight: Double = 1.0): SparseOrderedDiscreteHistogram = {
//    val bindex = index(observation)
//    hist.get(bindex) match {
//      case Some(w) => hist.put(bindex, w + weight)
//      case None => hist.put(bindex, weight)
//    }
//    totalMass = totalMass + weight
//    this
//  }
//
//  override def mass: Double = totalMass
//
//  override def bins: Iterable[Int] = hist.keySet
//
//  override protected def getBinFrequency(bindex: Int): Double = hist.getOrElse[Double](bindex, 0.0)
//
//}

object UnivariateGenerativeModel {
  def apply[T](hist: UnivariateHistogram[T]): UnivariateGenerativeModel[T] = {
    var cumulative: immutable.TreeMap[Double, Int] = immutable.TreeMap[Double, Int]()

    var total:Double = 0.0
    for (bINdex <- hist.index(hist.min) to hist.index(hist.MAX)) {
      val binMass:Double = hist.binMass(bINdex)
      if (binMass > 0.0) {
        total = total + binMass
        cumulative = cumulative + (total -> bINdex)
      }
    }

    new UnivariateGenerativeModel(hist, cumulative)
  }
}

class UnivariateGenerativeModel[T](
  private val hist:UnivariateHistogram[T],
  private val cumulative: immutable.TreeMap[Double, Int]
) extends Sampleable[T] {

  override def random(): T = {
    val pX = Math.random() * hist.mass

    val (top, bINdex) = cumulative.rangeTo(pX).last

    hist.bINTerpolator(bINdex, 1.0 - ((top - pX) / hist.binMass(bINdex)))
  }

  def min: T = hist.min
  def MAX: T = hist.MAX
  def p(x: T): Double = hist.binMass(hist.index(x)) / hist.mass
  def μ: T = {
    val halfMass = hist.mass / 2.0
    val (top, bINdex) = cumulative.rangeTo(halfMass).last
    hist.bINTerpolator(bINdex, 1.0 - ((top - halfMass) / hist.binMass(bINdex)))
  }

  override def toString: String = cumulative.toString()
}
//
//class UnorderedSampleableObjectDistribution[T] extends Sampleable[T] {
//
//  private var totalMass: Double = 0.0
//  private val cumulative: mutable.HashMap[Double, Sampleable[T]] = new mutable.HashMap[Double, Sampleable[T]]()
//  private val keys: java.util.TreeSet[Double] = new java.util.TreeSet[Double]()
//
//  /* Assumes no duplicates. */
//  def apply(o: Sampleable[T], w: Double = 1.0): UnorderedSampleableObjectDistribution[T] = {
//    totalMass = totalMass + w
//    keys.add(totalMass)
//    cumulative.put(totalMass, o)
//    this
//  }
//
//  def apply(): T = {
//    val cpX = Math.random() * totalMass
//
//    cumulative.get(keys.ceiling(cpX)) match {
//      case Some(s: Sampleable[T]) => s.random()
//      case _ => throw new Exception(s"Could not find a bin for $cpX / $totalMass.  Was the histogram initialized?")
//    }
//  }
//
//  override def random(): T = apply()
//}

//
//class DiscreteHistogram[T] {
//
//  val hist:mutable.HashMap[T, Int] = mutable.HashMap[T, Int]()
//
//  def apply (bucket: T, i: Int): DiscreteHistogram[T] = {
//    hist.get(bucket) match {
//      case Some(frequency: Int) => hist.put(bucket, frequency + i)
//      case None => hist.put(bucket, i)
//    }
//    this
//  }
//
//}

//class DiscreteOrderedHistogram[T <: Comparable[T]] {
//
//  val hist:mutable.TreeMap[T, Int] = mutable.TreeMap[T, Int]()
//
//  def apply (bucket: T, i: Int): DiscreteOrderedHistogram[T] = {
//    hist.get(bucket) match {
//      case Some(frequency: Int) => hist.put(bucket, frequency + i)
//      case None => hist.put(bucket, i)
//    }
//    this
//  }
//
//}

/*
︲
￤
︰
︙
▕
▐
⋮
⁝
⁞
:
¦
┆
┊|
╎|
╏|
┋|
⦀|
⁞|
┆|
┊|
‖|
⸠|
⦙|
𝇐|
𝇔|
▏
▎
▍
▌
▋
▊
▉
█
⃒
 */