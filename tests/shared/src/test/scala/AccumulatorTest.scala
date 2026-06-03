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

import slash.accumulation.{ContinuousAccumulator, DiscreteAccumulator}
import slash.squareInPlace

class AccumulatorTest extends munit.FunSuite {

  test("DiscreteAccumulator") {
    var bi:BigInt = BigInt(0)
    val da: DiscreteAccumulator = DiscreteAccumulator()
    var i: Int = 0; while (i < 100) {
      bi += BigInt(Long.MaxValue)
      da += Long.MaxValue
      bi += BigInt(i)
      da += i
      i += 1
    }
//    println(bd)
//    println(da.total)
    assertEquals(bi, da.total)
  }

//  Previous results:  Accumulator runs 7 times slower than Double, but hundreds of times faster than BigDecimal.
//  63640000000: BigDecimal time
//  221999872: Accumulator time
//  24000000: Double time
//
//  test("ContinuousAccumulator speed tests") {
//    var bd: BigDecimal = BigDecimal(0.0)
//    var i: Int = 0
//    var start:Long = System.nanoTime()
//    while (i < 1000000) {
//      bd += BigDecimal(Double.MinPositiveValue)
//      bd += BigDecimal(Math.random())
//      i += 1
//    }
//    var end = System.nanoTime() - start
//    println(s"$end: BigDecimal time")
//    start = System.nanoTime()
//    val ca: ContinuousAccumulator = ContinuousAccumulator()
//    i = 0
//    while (i < 1000000) {
//      ca += Double.MinPositiveValue
//      ca += Math.random() //Double.MaxValue
//      i += 1
//    }
//    end = System.nanoTime() - start
//    println(s"$end: Accumulator time")
//    start = System.nanoTime()
//    var d: Double = 0.0
//    i = 0
//    while (i < 1000000) {
//      d += Double.MinPositiveValue
//      d += Math.random() //Double.MaxValue
//      i += 1
//    }
//    end = System.nanoTime() - start
//    println(s"$end: Double time")
//    println(s"Comparisons: \n\t$bd: BigDecimal\n\t${ca.total}: Accumulator\n\t$d: Double")
//  }

  test("ContinuousAccumulator") {
    var bd: BigDecimal = BigDecimal(0.0)
    val ca: ContinuousAccumulator = ContinuousAccumulator()
    var d:Double = 0.0
    var i: Int = 0; while (i < 1000) {
      val rd:Double = Math.random() - 0.5
      val ri:Int = slash.Random.defaultRandom.nextInt()
      //bd += BigDecimal(Double.MinPositiveValue)
      bd += BigDecimal(ri)
      bd += BigDecimal(rd)
      ca += ri
      ca += rd
      d += ri
      d += rd
      i += 1
    }
//    println(bd remainder BigDecimal(1.0))
//    println(ca.total remainder BigDecimal(1.0))

//    println(s"$bd : BigDecimal")
//    println(s"${ca.total} : Accumulator")
//    println(s"$d : Double")

//    println(s"${bd.toDouble} : BigDecimal")
//    println(s"${ca.total.toDouble} : Accumulator")
//    println(s"$d : Double")

    assertEquals(ca.total.toDouble, bd.toDouble)

    // accumulator result should be closer to the BigDecimal than the plain Double
    assert(squareInPlace(bd - ca.total) <= squareInPlace(bd - d))

//    println(s"sum ${bd + bd}, ${(ca + bd).total}")
    assertEquals((bd + bd).toDouble, (ca + ca).total.toDouble)

//    println(s"product ${bd * bd}, ${(ca * ca).total}")
    assertEquals((bd * bd).toDouble, (ca * ca).total.toDouble)

//    println(s"quotient ${bd / bd}, ${(ca / ca).total}")
    assertEquals((bd / bd).toDouble, (ca / ca).total.toDouble)

//    println(s"quotient ${bd / BigDecimal(2.0)}, ${(ca / (ContinuousAccumulator() + BigDecimal(2.0))).total}")
    assertEquals((bd / BigDecimal(2.0)).toDouble, (ca / (ContinuousAccumulator() + BigDecimal(2.0))).total.toDouble)

  }

}