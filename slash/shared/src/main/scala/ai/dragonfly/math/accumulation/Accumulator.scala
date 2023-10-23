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

package ai.dragonfly.math.accumulation

trait Accumulator[T <: BigInt | BigDecimal] {
  def observe(n:T):Accumulator[T]
  def total:T

  def observe(b: Byte): Accumulator[T]

  def observe(s: Short): Accumulator[T]

  def observe(i: Int): Accumulator[T]

  def observe(l: Long): Accumulator[T]

  def observe(bi: BigInt): Accumulator[T]

  inline def observeProduct(n0: Long, n1: Long): this.type = (
    if (n0 == 0L || n1 == 0L) this
    else if (Long.MaxValue / n0 >= n1) observe(n0 * n1)
    else observe(BigInt(n0) * BigInt(n1))
  ).asInstanceOf[this.type]

}

object DiscreteAccumulator {
  val zero:BigInt = BigInt(0)
}

class DiscreteAccumulator extends Accumulator[BigInt] {
  private var big:BigInt = BigInt(0L)
  private var small:Long = Long.MaxValue
  private var dirty:Boolean = false

  private inline def smallSum:Long = Long.MaxValue - small
  private inline def resetSmallSum():Unit = {
    small = Long.MaxValue
    dirty = false
  }

  private def collapseAndGet():BigInt = {
    if (dirty) {
      big = big + BigInt(smallSum)
      resetSmallSum()
    }
    big
  }

  override def observe(bi:BigInt):DiscreteAccumulator = {
    big = big + bi
    dirty = true
    this
  }

  override def observe(l:Long): DiscreteAccumulator = {
    if (small >= l) small -= l
    else {
      big = big + BigInt(smallSum)
      small = Long.MaxValue - l
    }
    dirty = true
    this
  }

  override inline def observe(b:Byte):DiscreteAccumulator = observe(b.toLong)
  override inline def observe(s:Short):DiscreteAccumulator = observe(s.toLong)
  override inline def observe(i:Int):DiscreteAccumulator = observe(i.toLong)
  override inline def total: BigInt = collapseAndGet()

}

object ContinuousAccumulator {
  val longCutOff:Double = 9007199254740992.0
  val discreteCap:Double = Math.nextDown(Long.MaxValue.toDouble)
}

class ContinuousAccumulator extends Accumulator[BigDecimal] {

  import ContinuousAccumulator.*

  var discrete:DiscreteAccumulator = new DiscreteAccumulator
  var big: BigDecimal = BigDecimal(0L)
  var small: Double = 0.0
  var dirty:Boolean = false

  private def resetPartialSums():Unit = {
    small = 0.0
    discrete = new DiscreteAccumulator
    dirty = false
  }

  private def collapseAndGet(): BigDecimal = {
    if (dirty) {
      big += BigDecimal(small) + BigDecimal(discrete.total)
      resetPartialSums()
    }
    big
  }

  override def observe(bi: BigInt): ContinuousAccumulator = {
    discrete.observe(bi)
    dirty = true
    this
  }

  override def observe(bd: BigDecimal): ContinuousAccumulator = {
    big = big + bd
    dirty = true
    this
  }

  def observe(d: Double): ContinuousAccumulator = {

    if ( d < discreteCap) { // split discrete and fractional parts

      // discrete
      val l:Long = d.toLong
      discrete.observe(l)

      // fractional
      val dl:Double = d - l
      val temp:Double = small + dl
      if (temp == small) {  // no change?
        if (dl != 0.0) {  // d has no fractional part?
          big += BigDecimal(dl)
        } // no problem if dl is 0.0
      } else {
        small += d - l
      }

    } else {
      // This d value already probably suffers from precision loss.
      observe(BigDecimal(d))
    }
    dirty = true
    this
  }

  def observe(l: Long): ContinuousAccumulator = {
    discrete.observe(l)
    dirty = true
    this
  }

  override inline def observe(b: Byte): ContinuousAccumulator = observe(b.toLong)

  override inline def observe(s: Short): ContinuousAccumulator = observe(s.toLong)

  override inline def observe(i: Int): ContinuousAccumulator = observe(i.toLong)

  inline def observe(f: Float): ContinuousAccumulator = observe(f.toDouble)

  def observeProduct(n0: Double, n1: Double): ContinuousAccumulator = {
    if (longCutOff / n0 >= n1) observe(n0 * n1)
    else observe(BigDecimal(n0) * BigDecimal(n1))
  }
  override inline def total: BigDecimal = collapseAndGet()
}