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

package slash

package object interval {

  import narr.NArray
  import slash.interval.Interval.*

  def `[]`(min: Long, MAX: Long): LongInterval = LongInterval(CLOSED, min, MAX)

  def `(]`(min: Long, MAX: Long): LongInterval = LongInterval(RIGHT_CLOSED, min, MAX)

  def `[)`(min: Long, MAX: Long): LongInterval = LongInterval(LEFT_CLOSED, min, MAX)

  def `()`(min: Long, MAX: Long): LongInterval = LongInterval(OPEN, min, MAX)

  def `[]`(min: Int, MAX: Int): IntInterval = IntInterval(CLOSED, min, MAX)

  def `(]`(min: Int, MAX: Int): IntInterval = IntInterval(RIGHT_CLOSED, min, MAX)

  def `[)`(min: Int, MAX: Int): IntInterval = IntInterval(LEFT_CLOSED, min, MAX)

  def `()`(min: Int, MAX: Int): IntInterval = IntInterval(OPEN, min, MAX)

  def `[]`(min: Float, MAX: Float): FloatInterval = FloatInterval(CLOSED, min, MAX)

  def `(]`(min: Float, MAX: Float): FloatInterval = FloatInterval(RIGHT_CLOSED, min, MAX)

  def `[)`(min: Float, MAX: Float): FloatInterval = FloatInterval(LEFT_CLOSED, min, MAX)

  def `()`(min: Float, MAX: Float): FloatInterval = FloatInterval(OPEN, min, MAX)

  def `[]`(min: Double, MAX: Double): DoubleInterval = DoubleInterval(CLOSED, min, MAX)

  def `(]`(min: Double, MAX: Double): DoubleInterval = DoubleInterval(RIGHT_CLOSED, min, MAX)

  def `[)`(min: Double, MAX: Double): DoubleInterval = DoubleInterval(LEFT_CLOSED, min, MAX)

  def `()`(min: Double, MAX: Double): DoubleInterval = DoubleInterval(OPEN, min, MAX)

  transparent inline def `[]`[DOMAIN](min: DOMAIN, MAX: DOMAIN): Interval[DOMAIN] = of[DOMAIN](CLOSED, min, MAX)

  transparent inline def `(]`[DOMAIN](min: DOMAIN, MAX: DOMAIN): Interval[DOMAIN] = of[DOMAIN](RIGHT_CLOSED, min, MAX)

  transparent inline def `[)`[DOMAIN](min: DOMAIN, MAX: DOMAIN): Interval[DOMAIN] = of[DOMAIN](LEFT_CLOSED, min, MAX)

  transparent inline def `()`[DOMAIN](min: DOMAIN, MAX: DOMAIN): Interval[DOMAIN] = of[DOMAIN](OPEN, min, MAX)

  transparent inline def of[DOMAIN](code: Int, min: DOMAIN, MAX: DOMAIN): Interval[DOMAIN] = {
    (code match {
      case CLOSED => inline min match {
        case _: Int => `[]`(min.asInstanceOf[Int], MAX.asInstanceOf[Int])
        case _: Long => `[]`(min.asInstanceOf[Long], MAX.asInstanceOf[Long])
        case _: Float => `[]`(min.asInstanceOf[Float], MAX.asInstanceOf[Float])
        case _: Double => `[]`(min.asInstanceOf[Double], MAX.asInstanceOf[Double])
        case a => throw Exception(s"minOf[$a] expects numeric types {Int, Long, Float, Double}")
      }
      case RIGHT_CLOSED => inline min match {
        case _: Int => `(]`(min.asInstanceOf[Int], MAX.asInstanceOf[Int])
        case _: Long => `(]`(min.asInstanceOf[Long], MAX.asInstanceOf[Long])
        case _: Float => `(]`(min.asInstanceOf[Float], MAX.asInstanceOf[Float])
        case _: Double => `(]`(min.asInstanceOf[Double], MAX.asInstanceOf[Double])
        case a => throw Exception(s"minOf[$a] expects numeric types {Int, Long, Float, Double}")
      }
      case LEFT_CLOSED => inline min match {
        case _: Int => `[)`(min.asInstanceOf[Int], MAX.asInstanceOf[Int])
        case _: Long => `[)`(min.asInstanceOf[Long], MAX.asInstanceOf[Long])
        case _: Float => `[)`(min.asInstanceOf[Float], MAX.asInstanceOf[Float])
        case _: Double => `[)`(min.asInstanceOf[Double], MAX.asInstanceOf[Double])
        case a => throw Exception(s"minOf[$a] expects numeric types {Int, Long, Float, Double}")
      }
      case OPEN => inline min match {
        case _: Int => `()`(min.asInstanceOf[Int], MAX.asInstanceOf[Int])
        case _: Long => `()`(min.asInstanceOf[Long], MAX.asInstanceOf[Long])
        case _: Float => `()`(min.asInstanceOf[Float], MAX.asInstanceOf[Float])
        case _: Double => `()`(min.asInstanceOf[Double], MAX.asInstanceOf[Double])
        case a => throw Exception(s"minOf[$a] expects numeric types {Int, Long, Float, Double}")
      }
    }).asInstanceOf[Interval[DOMAIN]]
  }

  /**
   * Similar semantics to numpy linspace.
   * @param start starting value of numeric sequence
   * @param stop last value of sequence
   * @param num number of samples to generate.  Default is 50.
   * @returns NArray[Double]
   */
  def arithmeticProgression(start: Double, stop: Double, num: Int = 100): NArray[Double] = {
    assert(num > 1, s"num[$num] is not > 1")
    val arr = new NArray[Double](num)
    val dincr: Double = (stop - start) / (num-1).toDouble
    var d0: Double = start
    var i = 0
    while(i < num) { 
      arr(i) = d0
      d0 += dincr
      i += 1
    }
    arr
  }
}
