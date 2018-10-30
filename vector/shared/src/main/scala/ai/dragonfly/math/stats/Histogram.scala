package ai.dragonfly.math.stats

import ai.dragonfly.math.vector.Vector3

import scala.collection.mutable

trait Histogram {
  val binCount: Int
  val min: Double
  val max: Double
  val bucketWidth: Double
  var totalMass: Double
  def apply(x: Double, weight: Double = 1.0): Histogram
  def mass: Double
  def bins: Iterable[Int]
  def getBindex(observation: Double): Int = ((observation - min) / bucketWidth).toInt
  protected def getFrequency(bindex: Int): Double
  def getFrequency(query: Double): Double = getFrequency(getBindex(query))

  override def toString: String = {
    val sb = new mutable.StringBuilder("Histogram: { ")
    for (b <- bins) {
      sb.append(s"$b, ${getFrequency(b)/totalMass}\n")
    }
    sb.append("}")
    sb.toString()
  }
}


class DenseDiscreteHistogram(override val binCount: Int, override val min: Double, override val max: Double) extends Histogram {

  override val bucketWidth: Double = (max - min) / binCount
  println(bucketWidth)

  val hist: Array[Double] = Array.fill[Double](binCount)(0.0)
  override var totalMass: Double = 0.0

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
  override var totalMass = 0.0

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
  override var totalMass = 0.0

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

class UnorderedSampleableObjectDistribution[T <: Sampleable3] {

  private var totalMass: Double = 0.0
  private val cumulative: mutable.HashMap[Double, T] = new mutable.HashMap[Double, T]()
  private val keys: java.util.TreeSet[Double] = new java.util.TreeSet[Double]()

  /* Assumes no duplicates. */
  def apply(o: T, w: Double = 1.0): UnorderedSampleableObjectDistribution[T] = {
    totalMass = totalMass + w
    keys.add(totalMass)
    cumulative.put(totalMass, o)
    this
  }

  def apply(): Vector3 = {
    val cpX = Math.random() * totalMass

    cumulative.get(keys.ceiling(cpX)) match {
      case Some(s: Sampleable3) => s.draw()
      case _ => throw new Exception(s"Could not find a bin for $cpX / $totalMass.  Was the histogram initialized?")
    }
  }

}


class DiscreteHistogram[T] {

  val hist = mutable.HashMap[T, Int]()

  def apply (bucket: T, i: Int): DiscreteHistogram[T] = {
    hist.get(bucket) match {
      case Some(frequency: Int) => hist.put(bucket, frequency + i)
      case None => hist.put(bucket, i)
    }
    this
  }

}

class DiscreteOrderedHistogram[T <: Comparable[T]] {

  val hist = mutable.TreeMap[T, Int]()

  def apply (bucket: T, i: Int): DiscreteOrderedHistogram[T] = {
    hist.get(bucket) match {
      case Some(frequency: Int) => hist.put(bucket, frequency + i)
      case None => hist.put(bucket, i)
    }
    this
  }

}