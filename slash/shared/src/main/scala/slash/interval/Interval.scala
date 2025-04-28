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

package slash.interval

import slash.{nextDown, nextUp}
import slash.stats.probability.distributions.Sampleable

import scala.reflect.ClassTag
import scala.util.Random

object Interval {
  val OPEN:Int = 0x0
  val RIGHT_CLOSED:Int = 0x1
  val LEFT_CLOSED:Int = 0x1 << 1
  val CLOSED:Int = RIGHT_CLOSED | LEFT_CLOSED
}

object LongInterval {
  def apply(code:Int, min:Long, MAX:Long):LongInterval = {
    if (MAX < min) throw Exception(s"Invalid sampleRange: [min = $min, MAX = $MAX] MAX is less than min.")
    else new LongInterval(code, min, MAX)
  }
}

case class LongInterval private (override val code:Int, override val min:Long, override val MAX:Long) extends DiscreteInterval[Long] {
  override def norm: Long = MAX - min
  override def median: Long = (MAX - min) >> 1
  override def mean: Double = (set_MAX + set_min) / 2.0

  override def contains(x: Long): Boolean = x <= MAX && x >= min
  override def rangeContains(x:Double): Boolean = x <= MAX.toDouble && x >= min.toDouble
  override def random(r0: Random): Long = r0.between(min, MAX + 1)
}


object IntInterval {
  def apply(code:Int, min:Int, MAX:Int):IntInterval = {
    if (MAX < min) throw Exception(s"Invalid sampleRange: [min = $min, MAX = $MAX] MAX is less than min.")
    else new IntInterval(code, min, MAX)
  }
}

case class IntInterval private (override val code:Int, override val min:Int, override val MAX:Int) extends DiscreteInterval[Int] {
  override def norm: Int = MAX - min
  override def median: Int = (MAX - min) >> 1
  override def mean: Double = (set_MAX + set_min) / 2.0

  override inline def contains(x: Int): Boolean = x <= MAX && x >= min
  override def rangeContains(x:Double): Boolean = x <= MAX.toDouble && x >= min.toDouble

  override inline def random(r0: Random): Int = r0.between(min, MAX + 1)
}

object ContinuousInterval {
  def apply(code:Int, min:Double, MAX:Double):ContinuousInterval = {
    if (MAX < min) throw Exception(s"Invalid sampleRange: [min = $min, MAX = $MAX] MAX is less than min.")
    else new ContinuousInterval(code, min, MAX)
  }
}

case class ContinuousInterval private (override val code:Int, override val min:Double, override val MAX:Double) extends Interval[Double] {
  override def norm: Double = MAX - min
  override def mean: Double = (MAX - min) / 2.0
  override inline def contains(x: Double): Boolean = x <= MAX && x >= min
  override inline def rangeContains(x:Double):Boolean = contains(x)
  override def random(r0: Random): Double = r0.between(min, if (MAX < Double.MaxValue) MAX.nextUp else MAX)
}


object FloatInterval {
  def apply(code:Int, min:Float, MAX:Float):FloatInterval = {
    if (MAX < min) throw Exception(s"Invalid sampleRange: [min = $min, MAX = $MAX] MAX is less than min.")
    else new FloatInterval(code, min, MAX)
  }
}

case class FloatInterval private (override val code:Int, override val min:Float, override val MAX:Float) extends Interval[Float] {
  override def norm: Float = MAX - min
  override def mean: Double = (MAX - min) / 2.0
  override inline def contains(x: Float): Boolean = x <= MAX && x >= min
  override inline def rangeContains(x:Double):Boolean = x <= MAX.toDouble && x >= min.toDouble
  override def random(r0: Random): Float = r0.between(min, MAX.nextUp)
}

trait DiscreteInterval[DOMAIN <: Int | Long : ClassTag] extends Interval[DOMAIN] {
  def median: DOMAIN
}


trait Interval[DOMAIN:ClassTag] extends Sampleable[DOMAIN] {

  val code:Int

  val min:DOMAIN
  val MAX:DOMAIN
  def norm:DOMAIN
  def mean:Double

  lazy val set_min:DOMAIN = if (leftClosed) min else nextUp[DOMAIN](min)
  lazy val set_MAX:DOMAIN = if (rightClosed) MAX else nextDown[DOMAIN](MAX)

  lazy val open: Boolean = code == Interval.OPEN
  lazy val leftOpen: Boolean = (code >> 1) == 0
  lazy val rightOpen: Boolean = (code & 0x00000001) == 0
  lazy val closed: Boolean = code == Interval.CLOSED
  lazy val leftClosed: Boolean = (code >> 1) > 0
  lazy val rightClosed: Boolean = (code & 0x00000001) > 0

  def contains(x:DOMAIN):Boolean // =  (if (leftClosed) x >= min else x > min) && (if (rightClosed) x <= MAX else x < MAX)

  def rangeContains(x: Double): Boolean // = Interval.rangeContains(this, x)

  import slash.Random.*

  override def random(r0:scala.util.Random = defaultRandom): DOMAIN// = Interval.randomFrom(this).asInstanceOf[DOMAIN]

  override def toString:String = s"${if(leftClosed) "[" else "("} $min, $MAX ${if(rightClosed) "]" else ")" }${ClassTag[DOMAIN]}"

}
