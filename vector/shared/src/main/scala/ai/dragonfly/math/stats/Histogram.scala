package ai.dragonfly.math.stats

import ai.dragonfly.math.util.Demonstrable

import scala.collection.mutable

object Histogram extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    val sodh: Histogram = new SparseOrderedDiscreteHistogram(10, 0.0, 10.0)
    for (i <- 0 until 10000) {
      sodh(
        10.0 * Math.random(),
        Math.random()
      )
    }
    sb.append(sodh)

    val gm = UnivariateGenerativeModel(sodh)

    sb.append(gm)

    val sodh1: Histogram = new SparseOrderedDiscreteHistogram(10, 0.0, 10.0)

    for (i <- 0 until 10000) {
      sodh1( gm(), 1.0 )
    }

    sb.append(sodh1)

    val sodh2: Histogram = new SparseOrderedDiscreteHistogram(10, 0.0, 10.0)

    sodh2(1 + Math.random(), 5)
    sodh2(2 + Math.random(), 39)
    sodh2(6 + Math.random(), 15)
    sodh2(9 + Math.random(), 25)

    val gm1 = UnivariateGenerativeModel(sodh2)

    val sodh3: Histogram = new SparseOrderedDiscreteHistogram(10, 0.0, 10.0)

    for (i <- 0 until 10000) sodh3( gm1(), 1.0 )

    sb.append(s"$sodh2\n$sodh3")

    // DenseDiscreteHistogram
    val ddh: Histogram = new DenseDiscreteHistogram(10, -10.0, 10.0)

    for (i <- 0 until 10000) {
      val observation = 20 * (Math.random() - 0.5)
      ddh(observation)
      //sb.append(s"ddh.getBindex($observation) returns ${ddh.getBindex(observation)}")
    }

    val gm2 = UnivariateGenerativeModel(ddh)

    val ddh1: Histogram = new DenseDiscreteHistogram(10, -10.0, 10.0)

    for (i <- 0 until 10000) {
      val obs = gm2()
      ddh1( obs )
    }

    //  for (i <- 0 until 10) sb.append(gm2())

    sb.append(s"$ddh\n$ddh1")

    val sdh: Histogram = new SparseDiscreteHistogram(10, 0.0, 10.0)

    sdh(1 + Math.random(), 5)
    sdh(2 + Math.random(), 39)
    sdh(6 + Math.random(), 15)
    sdh(9 + Math.random(), 25)

    val gm3 = UnivariateGenerativeModel(sdh)

    val sdh1: Histogram = new SparseDiscreteHistogram(10, 0.0, 10.0)

    for (i <- 0 until 10000) sdh1(gm3(), 1.0)

    sb.append(s"$sdh\n$sdh1")
  }

  override def name: String = "Histogram"
}

trait Histogram {
  val binCount: Int
  val min: Double
  val max: Double
  val bucketWidth: Double
  def getTotalMass: Double
  def apply(x: Double, weight: Double = 1.0): Histogram
  def mass: Double
  def bins: Iterable[Int]
  def getBindex(observation: Double): Int = Math.min((observation - min) / bucketWidth, binCount - 1.0).toInt
  protected def getFrequency(bindex: Int): Double
  def getFrequency(query: Double): Double = getFrequency(getBindex(query))
  def bucketLabel(b:Int):Double = min + ((1000.0 * ((b*bucketWidth) + (0.5 * bucketWidth))).toInt / 1000.0)

  override def toString: String = {
    val sb = new mutable.StringBuilder("Histogram: { \n")
    for (b <- bins) {
      sb.append("\t").append(bucketLabel(b)).append(",").append(getFrequency(b)/getTotalMass).append("\n")
    }
    sb.append("}")
    sb.toString()
  }
}


class DenseDiscreteHistogram(override val binCount: Int, override val min: Double, override val max: Double) extends Histogram {

  override val bucketWidth: Double = (max - min) / binCount
  //println(bucketWidth)

  val hist: Array[Double] = Array.fill[Double](binCount)(0.0)
  private var totalMass: Double = 0.0
  def getTotalMass:Double = totalMass

  def apply(observation: Double, weight: Double = 1.0): DenseDiscreteHistogram = {
    val bindex = getBindex(observation)

    hist(bindex) = hist(bindex) + weight
    totalMass = totalMass + weight

    this
  }

  override def mass: Double = totalMass

  def bins: Iterable[Int] = hist.indices.toArray

  override protected def getFrequency(bindex: Int): Double = hist(bindex)
}

class SparseDiscreteHistogram(override val binCount: Int, override val min: Double, override val max: Double) extends Histogram {

  override val bucketWidth: Double = (max - min) / binCount

  val hist: mutable.HashMap[Int, Double] = mutable.HashMap[Int, Double]()
  private var totalMass = 0.0
  def getTotalMass:Double = totalMass
  def apply(observation: Double, weight: Double = 1.0): SparseDiscreteHistogram = {
    val bindex = getBindex(observation)

    hist.get(bindex) match {
      case Some(frequency: Double) => hist.put(bindex, frequency + weight)
      case None => hist.put(bindex, weight)
    }
    totalMass = totalMass + weight
    this
  }

  override def mass: Double = totalMass

  override def bins: Iterable[Int] = hist.keySet

  override protected def getFrequency(bindex: Int): Double = hist.getOrElse[Double](bindex, 0.0)
}

class SparseOrderedDiscreteHistogram(override val binCount: Int, override val min: Double, override val max: Double) extends Histogram {

  override val bucketWidth: Double = (max - min) / binCount

  val hist: mutable.TreeMap[Int, Double] = mutable.TreeMap[Int, Double]()
  private var totalMass = 0.0
  override def getTotalMass: Double = totalMass
  def apply(observation: Double, weight: Double = 1.0): SparseOrderedDiscreteHistogram = {
    val bindex = getBindex(observation)
    hist.get(bindex) match {
      case Some(w) => hist.put(bindex, w + weight)
      case None => hist.put(bindex, weight)
    }
    totalMass = totalMass + weight
    this
  }

  override def mass: Double = totalMass

  override def bins: Iterable[Int] = hist.keySet

  override protected def getFrequency(bindex: Int): Double = hist.getOrElse[Double](bindex, 0.0)

}

object UnivariateGenerativeModel {
  def apply(hist: Histogram): UnivariateGenerativeModel = {
    val cumulative: mutable.HashMap[Double, Int] = new mutable.HashMap[Double, Int]

    var total = 0.0
    for (bin <- hist.bins) {
      val f = hist.getFrequency(bin)
      if (f > 0.0) {
        total = total + f
        cumulative.put(total, bin)
      }
    }

    new UnivariateGenerativeModel(hist.bucketWidth, hist.min, total, cumulative)
  }
}

class UnivariateGenerativeModel(
  private val bucketWidth: Double,
  private val min: Double,
  private val totalMass: Double,
  private val cumulative: mutable.HashMap[Double, Int]
) {

  val keys: java.util.TreeSet[Double] = new java.util.TreeSet[Double]()
  for (k <- cumulative.keys) keys.add(k)

  def apply(): Double = {
    val cpX = Math.random() * totalMass

    val high = keys.ceiling(cpX)
    val low = keys.floor(cpX)

    //println(s"$cpX => $low, $high, ${cumulative.getOrElse(low, 1)}")
    val remainder = (cpX - low) / (high - low)

    val bin = cumulative.getOrElse(high, 0)
    ((bin + remainder) * bucketWidth) + min
  }

  override def toString: String = cumulative.toString()
}

class UnorderedSampleableObjectDistribution[T] extends Sampleable[T] {

  private var totalMass: Double = 0.0
  private val cumulative: mutable.HashMap[Double, Sampleable[T]] = new mutable.HashMap[Double, Sampleable[T]]()
  private val keys: java.util.TreeSet[Double] = new java.util.TreeSet[Double]()

  /* Assumes no duplicates. */
  def apply(o: Sampleable[T], w: Double = 1.0): UnorderedSampleableObjectDistribution[T] = {
    totalMass = totalMass + w
    keys.add(totalMass)
    cumulative.put(totalMass, o)
    this
  }

  def apply(): T = {
    val cpX = Math.random() * totalMass

    cumulative.get(keys.ceiling(cpX)) match {
      case Some(s: Sampleable[T]) => s.random()
      case _ => throw new Exception(s"Could not find a bin for $cpX / $totalMass.  Was the histogram initialized?")
    }
  }

  override def random(): T = apply()
}


class DiscreteHistogram[T] {

  val hist:mutable.HashMap[T, Int] = mutable.HashMap[T, Int]()

  def apply (bucket: T, i: Int): DiscreteHistogram[T] = {
    hist.get(bucket) match {
      case Some(frequency: Int) => hist.put(bucket, frequency + i)
      case None => hist.put(bucket, i)
    }
    this
  }

}

class DiscreteOrderedHistogram[T <: Comparable[T]] {

  val hist:mutable.TreeMap[T, Int] = mutable.TreeMap[T, Int]()

  def apply (bucket: T, i: Int): DiscreteOrderedHistogram[T] = {
    hist.get(bucket) match {
      case Some(frequency: Int) => hist.put(bucket, frequency + i)
      case None => hist.put(bucket, i)
    }
    this
  }

}