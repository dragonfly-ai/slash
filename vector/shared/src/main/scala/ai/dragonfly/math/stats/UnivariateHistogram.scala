package ai.dragonfly.math.stats

import ai.dragonfly.math.stats.probability.distributions.{ProbabilityDistribution, Sampleable}
import ai.dragonfly.math.examples.Demonstrable

import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag


trait UnivariateHistogram[T] {

  val size: Int // number of bins
  val min: T
  val MAX: T
  def apply(x: T, weight: Double = 1.0): UnivariateHistogram[T]
  def mass: Double
  def index(x:T):Int
  def bINTerpolator(bINdex:Int, alpha:Double):T
  def binMass(bINdex:Int):Double
  def binLabel(bINdex:Int):String

  override def toString: String = {
    val eightBlocks: Array[String] = Array[String](" ", "▏", "▎", "▍", "▌", "▋", "▊", "▉", "█" )
    val sb = new mutable.StringBuilder("Histogram:").append(" { \n")
    for (bIndex <- index(min) to index(MAX)) {
      val bnms:Double = binMass(bIndex)/mass
      val m = (1000.0 * bnms).toInt
      sb.append("\t").append(binLabel(bIndex)).append(" ")
      for (i <- 0 until (m/8)) {
        sb.append("█") // ░
      }
      sb.append(eightBlocks(m % 8))
      sb.append(s" = $bnms").append("\n")
    }
    sb.append("}")
    sb.toString()
  }

}

class DenseHistogramOfDiscreteDistribution(override val size: Int, override val min: Long, override val MAX: Long) extends UnivariateHistogram[Long] {

  val bucketWidth: Double = (MAX - min).toDouble / size.toDouble

  val hist: Array[Double] = Array.fill(size+1)(0.0)

  private var totalMass: Double = 0.0

  def apply(observation: Long, weight: Double): DenseHistogramOfDiscreteDistribution = {
    val bindex = index(observation)

    hist(bindex) = hist(bindex) + weight
    totalMass = totalMass + weight

    this
  }

  override def mass: Double = totalMass

  def binMass(bINdex: Int): Double = hist(bINdex)

  private val LeftOfDecimal: Int = (Math.log10(MAX) + 1.0).toInt
  private val RightOfDecimal: Int = (Math.log10( if (bucketWidth < 1.0) (1.0 / bucketWidth) else (100.0 / bucketWidth)) + 1.0).toInt

  override def index(x: Long): Int = Math.floor((x - min)/bucketWidth).toInt

  override def bINTerpolator(bINdex: Int, alpha: Double): Long = {
    ((alpha * (min + (bINdex * bucketWidth))) + ((1.0 - alpha) * (min + ((bINdex + 1) * bucketWidth)))).toLong
  }

  override def binLabel(bINdex:Int):String = {
    (s"%1${LeftOfDecimal}.${RightOfDecimal}f").format(min + (bINdex * bucketWidth))
  }
}

class DenseHistogramOfContinuousDistribution(override val size: Int, override val min: Double, override val MAX: Double) extends UnivariateHistogram[Double] {

  val bucketWidth: Double = (MAX - min) / size

  val hist: Array[Double] = Array.fill(size+1)(0.0)

  private var totalMass: Double = 0.0

  def apply(observation: Double, weight: Double): DenseHistogramOfContinuousDistribution = {
    val bindex = index(observation)

    hist(bindex) = hist(bindex) + weight
    totalMass = totalMass + weight

    this
  }

  override def mass: Double = totalMass

  override def binMass(bINdex: Int): Double = hist(bINdex)

  private val LeftOfDecimal: Int = (Math.log10(MAX) + 1.0).toInt
  private val RightOfDecimal: Int = (Math.log10( if (bucketWidth < 1.0) (1.0 / bucketWidth) else (100.0 / bucketWidth)) + 1.0).toInt

  override def index(x: Double): Int = Math.floor((x - min)/bucketWidth).toInt

  override def bINTerpolator(bINdex: Int, alpha: Double): Double = {
    (alpha * (min + (bINdex * bucketWidth))) + ((1.0 - alpha) * (min + ((bINdex + 1) * bucketWidth)))
  }

  override def binLabel(bINdex:Int):String = {
    (s"%1${LeftOfDecimal}.${RightOfDecimal}f").format(min + (bINdex * bucketWidth))
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