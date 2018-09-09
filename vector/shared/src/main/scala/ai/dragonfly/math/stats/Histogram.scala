package ai.dragonfly.math.stats

import scala.collection.mutable

trait Histogram {
  val binCount: Int
  val min: Double
  val max: Double
  val bucketWidth: Double
  var totalMass: Double
  def apply(x: Double, weight: Double): Histogram
  def mass: Double
  def bins: Iterable[Int]
  def getBindex(observation: Double): Int = ((observation - min) / bucketWidth).toInt
  protected def getFrequency(bindex: Int): Double
  def getFrequency(query: Double): Double = getFrequency(getBindex(query))

  override def toString(): String = {
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

  val hist: Array[Double] = Array.fill[Double](binCount)(0.0)
  override var totalMass: Double = 0.0

  def apply(observation: Double, weight: Double = 1.0): DenseDiscreteHistogram = {
    val bindex = getBindex(observation)
    hist(bindex) = hist(bindex) + weight
    totalMass = totalMass + weight
    this
  }

  override def mass: Double = totalMass

  def bins: Iterable[Int] = {
    val arr = hist.indices.toArray
    println(hist.indices)
    for (i <- arr) print(s"$i, ")
    arr
  }

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

    ((cumulative.getOrElse(high, 0) + remainder) * bucketWidth) - min
  }

  override def toString(): String = cumulative.toString()
}