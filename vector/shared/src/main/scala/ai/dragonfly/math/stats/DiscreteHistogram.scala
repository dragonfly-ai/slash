package ai.dragonfly.math.stats

import scala.collection.mutable

class DiscreteHistogram[T] {

  val hist = mutable.HashMap[T, Int]()

  def adjust (bucket: T, i: Int): DiscreteHistogram[T] = {
    hist.get(bucket) match {
      case Some(frequency: Int) => hist.put(bucket, frequency + i)
      case None => hist.put(bucket, i)
    }
    this
  }

}

class DiscreteOrderedHistogram[T <: Comparable[T]] {

  val hist = mutable.TreeMap[T, Int]()

  def adjust (bucket: T, i: Int): DiscreteOrderedHistogram[T] = {
    hist.get(bucket) match {
      case Some(frequency: Int) => hist.put(bucket, frequency + i)
      case None => hist.put(bucket, i)
    }
    this
  }

}
