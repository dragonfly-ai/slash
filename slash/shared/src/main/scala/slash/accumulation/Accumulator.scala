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

package slash.accumulation

trait Accumulator[T <: BigInt | BigDecimal] {

  type AT <: Accumulator[T]
  def total:T

  def copy:AT

  def + (b: Byte): AT = {
    val out = this.copy
    out += b
    out.asInstanceOf[AT]
  }
  def + (s: Short): AT = {
    val out = this.copy
    out += s
    out.asInstanceOf[AT]
  }
  def + (i: Int): AT = {
    val out = this.copy
    out += i
    out.asInstanceOf[AT]
  }
  def + (l: Long): AT = {
    val out = this.copy
    out += l
    out.asInstanceOf[AT]
  }
  def + (bi: BigInt): AT = {
    val out = this.copy
    out += bi
    out.asInstanceOf[AT]
  }

  def +=(b: Byte): Unit
  def +=(s: Short): Unit
  def +=(i: Int): Unit
  def +=(l: Long): Unit
  def +=(bi: BigInt): Unit

  def -(b: Byte): AT = {
    val out = this.copy
    out -= b
    out.asInstanceOf[AT]
  }

  def -(s: Short): AT = {
    val out = this.copy
    out -= s
    out.asInstanceOf[AT]
  }

  def -(i: Int): AT = {
    val out = this.copy
    out -= i
    out.asInstanceOf[AT]
  }

  def -(l: Long): AT = {
    val out = this.copy
    out -= l
    out.asInstanceOf[AT]
  }

  def -(bi: BigInt): AT = {
    val out = this.copy
    out -= bi
    out.asInstanceOf[AT]
  }

  def -=(b: Byte): Unit

  def -=(s: Short): Unit

  def -=(i: Int): Unit

  def -=(l: Long): Unit

  def -=(bi: BigInt): Unit


  def +(a: DiscreteAccumulator): AT
  def +(a: ContinuousAccumulator): ContinuousAccumulator

  def -(a: DiscreteAccumulator): AT
  def -(a: ContinuousAccumulator): ContinuousAccumulator

  def *(a: DiscreteAccumulator): AT
  def *(a: ContinuousAccumulator): ContinuousAccumulator

  def /(a: DiscreteAccumulator): ContinuousAccumulator
  def /(a: ContinuousAccumulator): ContinuousAccumulator

  def +=(a: DiscreteAccumulator): Unit
  def -=(a: DiscreteAccumulator): Unit

  inline def observeProduct(n0: Long, n1: Long): Unit = {
    if (n0 == 0L || n1 == 0L) ()
    else if (Long.MaxValue / n0 >= n1) this += (n0 * n1)
    else this += (BigInt(n0) * BigInt(n1))
  }
}

object DiscreteAccumulator {
  val zero:BigInt = BigInt(0)
  val maxLong:BigInt = BigInt(Long.MaxValue)
  val minLong:BigInt = BigInt(Long.MinValue)
  def apply():DiscreteAccumulator = new DiscreteAccumulator()
}

class DiscreteAccumulator private (var overflowCount:BigInt = BigInt(0), var small:Long = 0L) extends Accumulator[BigInt] {

  override type AT = DiscreteAccumulator

  private def collapseAndGet():BigInt = {
    if (overflowCount > 0) (DiscreteAccumulator.maxLong * overflowCount) + BigInt(small)
    else BigInt(small)
  }

  override def += (bi:BigInt):Unit = {
    if (bi < DiscreteAccumulator.maxLong && bi > DiscreteAccumulator.minLong) this += bi.toLong
    else {
      overflowCount += bi / DiscreteAccumulator.maxLong
      this += (bi % DiscreteAccumulator.maxLong).toLong
    }
  }

  override def += (l:Long): Unit = {
    if (l > 0L) {
      if (small > 0L) {
        val tolerance: Long = Long.MaxValue - small
        if (tolerance > l) small += l
        else {
          overflowCount += 1
          small = l - tolerance
        }
      } else if (small < 0L) {
        small += l
      } else small = l
    } else if (l < 0L) {
      if (small > 0L) {
        small += l
      } else {
        val tolerance:Long = Long.MinValue - small
        if (tolerance < l) small += l
        else {
          overflowCount -= 1
          small = l - tolerance
        }
      }
    }
  }

  override inline def +=(b:Byte):Unit = this += b.toLong
  override inline def += (s:Short):Unit = this += s.toLong
  override inline def += (i:Int):Unit = this += i.toLong


  override inline def -=(b: Byte): Unit = this += (-b).toLong
  override inline def -=(s: Short): Unit = this += (-s).toLong
  override inline def -=(i: Int): Unit = this += (-i).toLong
  override inline def -=(l: Long): Unit = this += -l
  override inline def -=(bi: BigInt): Unit = this += (bi * BigInt(-1))

  override inline def total: BigInt = collapseAndGet()

  override def copy: AT = new DiscreteAccumulator( overflowCount, small )

  override def +(a: DiscreteAccumulator): DiscreteAccumulator = {
    val out = new DiscreteAccumulator(this.overflowCount + a.overflowCount, small)
    out += a.small
    out
  }

  override def +(a: ContinuousAccumulator): ContinuousAccumulator = a + this

  override def -(a: DiscreteAccumulator): DiscreteAccumulator = {
    val out = new DiscreteAccumulator(this.overflowCount - a.overflowCount, small)
    out += a.small
    out
  }

  override def -(a: ContinuousAccumulator): ContinuousAccumulator = ContinuousAccumulator(this.copy) - a

  override def *(a: DiscreteAccumulator): DiscreteAccumulator = DiscreteAccumulator() + (this.total * a.total)

  override def *(a: ContinuousAccumulator): ContinuousAccumulator = ContinuousAccumulator() + (BigDecimal(this.total) * a.total)

  override def /(a: DiscreteAccumulator): ContinuousAccumulator = ContinuousAccumulator() + (BigDecimal(this.total) / BigDecimal(a.total))

  override def /(a: ContinuousAccumulator): ContinuousAccumulator = ContinuousAccumulator() + (BigDecimal(this.total) / a.total)

  override def +=(a: DiscreteAccumulator): Unit = {
    overflowCount += a.overflowCount
    this += a.small
  }

  override def -=(a: DiscreteAccumulator): Unit = {
    overflowCount -= a.overflowCount
    this -= a.small
  }

}

object ContinuousAccumulator {
  val LongCutOff:Double = slash.Constant.MaxContiguousLong.toDouble
  val One:BigDecimal = BigDecimal(1.0)
  val NegativeOne:BigDecimal = BigDecimal(-1.0)

  def apply(
    d: DiscreteAccumulator = DiscreteAccumulator(),
    s:Double = 0.0,
    e: Double = 0.0
  ):ContinuousAccumulator = new ContinuousAccumulator(d, s, e)
}

class ContinuousAccumulator private (
  var discrete: DiscreteAccumulator = DiscreteAccumulator(),
  var small: Double = 0.0,
  var error: Double = 0.0
) extends Accumulator[BigDecimal] {

  override type AT = ContinuousAccumulator

  import ContinuousAccumulator.*

  def calculateError(a: Double, b: Double, sum: Double): Double = {
    val bVirtual = sum - a
    (a - (sum - bVirtual)) + (b - bVirtual)
  }

  private inline def addToSmall(dl:Double):Unit = {
    val temp: Double = small + dl
    error += calculateError(small, dl, temp)
    if (temp < 1.0) small = temp
    else {
      val tl:Long = temp.toLong
      discrete += tl
      small = temp - tl.toDouble
    }
  }

  override inline def +=(b: Byte): Unit = discrete += b
  override inline def +=(s: Short): Unit = discrete += s
  override inline def +=(i: Int): Unit = discrete += i
  override inline def += (bi: BigInt): Unit = discrete += bi

  inline def += (bd: BigDecimal): Unit = {
    if (bd >= One || bd <= NegativeOne) discrete += bd.toBigInt
    this += bd.remainder(One).toDouble // bite me!
  }

  override def +=(l: Long): Unit = discrete += l

  def + (f: Float): ContinuousAccumulator = {
    val out = this.copy
    out += f.toDouble
    out
  }

  def +(d: Double): ContinuousAccumulator = {
    val out = this.copy
    out += d
    out
  }

  def +(bd: BigDecimal): ContinuousAccumulator = {
    val out = this.copy
    out += bd
    out
  }

  inline def +=(f: Float): Unit = this += f.toDouble
  def += (d: Double): Unit = {

    // split discrete and fractional parts
    if (d < LongCutOff && d > -LongCutOff) {
      discrete += d.toLong // discrete
      addToSmall(d % 1.0) // fractional
    } else {
      // This d value already probably suffers from precision loss.
      this += BigDecimal(d)
    }
  }

  def -(d: Double): ContinuousAccumulator = {
    val out = this.copy
    out -= d
    out
  }

  def -=(d: Double): Unit = {

    // split discrete and fractional parts
    if (d < LongCutOff && d > -LongCutOff) {
      discrete -= d.toLong  // discrete
      addToSmall(-d % 1.0)  // fractional
    } else {
      // This d value already probably suffers from precision loss.
      this -= BigDecimal(d)
    }
  }

  inline def -=(bd: BigDecimal): Unit = {
    if (bd >= One || bd <= NegativeOne) discrete -= bd.toBigInt
    this -= (bd.remainder(One)).toDouble // bite me!
  }

  override def -=(l: Long): Unit = discrete -= l

  def +=(a: ContinuousAccumulator): Unit = {
    this.discrete += a.discrete
    this += (a.small + a.error)
  }
  def -=(a: ContinuousAccumulator): Unit = {
    this.discrete += a.discrete
    this += (a.small + a.error)
  }

  def observeProduct(n0: Double, n1: Double): Unit = {
    if (LongCutOff / n0 >= n1) this += (n0 * n1)
    else this += (BigDecimal(n0) * BigDecimal(n1))
  }

  override inline def total: BigDecimal = BigDecimal(error) + BigDecimal(small) + BigDecimal(discrete.total)

  override def copy: ContinuousAccumulator = new ContinuousAccumulator(discrete.copy, small, error)

  override inline def -=(b: Byte): Unit = discrete -= b
  override inline def -=(s: Short): Unit = discrete -= s
  override inline def -=(i: Int): Unit = discrete -= i
  override inline def -=(bi: BigInt): Unit = discrete -= bi

  override inline def +(a: DiscreteAccumulator): ContinuousAccumulator = {
    val t = ContinuousAccumulator(this.discrete + a, small, error)
    t += a.small
    t
  }

  override def +(a: ContinuousAccumulator): ContinuousAccumulator = {
    val out = this.copy
    out.discrete += a.discrete
    out += (a.small + a.error)
    out
  }
  override def -(a: DiscreteAccumulator): ContinuousAccumulator = ContinuousAccumulator(this.discrete - a, small, error)
  override def -(a: ContinuousAccumulator): ContinuousAccumulator = {
    val out = this.copy
    out.discrete -= a.discrete
    out -= (a.small + a.error)
    out
  }

  override def *(a: DiscreteAccumulator): ContinuousAccumulator = a * this

  override def *(a: ContinuousAccumulator): ContinuousAccumulator = ContinuousAccumulator() + (this.total * a.total)

  override def /(a: DiscreteAccumulator): ContinuousAccumulator = ContinuousAccumulator() + (this.total / BigDecimal(a.total))

  override def /(a: ContinuousAccumulator): ContinuousAccumulator = ContinuousAccumulator() + (this.total / a.total)

  override def +=(a: DiscreteAccumulator): Unit = this.discrete += a

  override def -=(a: DiscreteAccumulator): Unit = this.discrete -= a
}