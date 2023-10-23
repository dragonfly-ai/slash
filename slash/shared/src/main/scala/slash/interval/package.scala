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

  def `[]`(min: Double, MAX: Double): ContinuousInterval = ContinuousInterval(CLOSED, min, MAX)

  def `(]`(min: Double, MAX: Double): ContinuousInterval = ContinuousInterval(RIGHT_CLOSED, min, MAX)

  def `[)`(min: Double, MAX: Double): ContinuousInterval = ContinuousInterval(LEFT_CLOSED, min, MAX)

  def `()`(min: Double, MAX: Double): ContinuousInterval = ContinuousInterval(OPEN, min, MAX)

  transparent inline def `[]`[DOMAIN](min: DOMAIN, MAX: DOMAIN): Interval[DOMAIN] = of[DOMAIN](CLOSED, min, MAX)

  transparent inline def `(]`[DOMAIN](min: DOMAIN, MAX: DOMAIN): Interval[DOMAIN] = of[DOMAIN](RIGHT_CLOSED, min, MAX)

  transparent inline def `[)`[DOMAIN](min: DOMAIN, MAX: DOMAIN): Interval[DOMAIN] = of[DOMAIN](LEFT_CLOSED, min, MAX)

  transparent inline def `()`[DOMAIN](min: DOMAIN, MAX: DOMAIN): Interval[DOMAIN] = of[DOMAIN](OPEN, min, MAX)

  transparent inline def of[DOMAIN](code: Int, min: DOMAIN, MAX: DOMAIN): Interval[DOMAIN] = inline min match {
    //    case _: Byte => `[]`(min.asInstanceOf[Byte], MAX.asInstanceOf[Byte]).asInstanceOf[Interval[DOMAIN]]
    //    case _: Short => `[]`(min.asInstanceOf[Short], MAX.asInstanceOf[Short]).asInstanceOf[Interval[DOMAIN]]
    case _: Int => `[]`(min.asInstanceOf[Int], MAX.asInstanceOf[Int]).asInstanceOf[Interval[DOMAIN]]
    case _: Long => `[]`(min.asInstanceOf[Long], MAX.asInstanceOf[Long]).asInstanceOf[Interval[DOMAIN]]
    case _: Float => `[]`(min.asInstanceOf[Float], MAX.asInstanceOf[Float]).asInstanceOf[Interval[DOMAIN]]
    case _: Double => `[]`(min.asInstanceOf[Double], MAX.asInstanceOf[Double]).asInstanceOf[Interval[DOMAIN]]
    case a => throw Exception(s"minOf[$a] expects numeric types")
  }
}
