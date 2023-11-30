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

  test("ContinuousAccumulator") {
    var bd: BigDecimal = BigDecimal(0.0)
    val ca: ContinuousAccumulator = ContinuousAccumulator()
    var i: Int = 0; while (i < 1) {
      bd += BigDecimal(Double.MinPositiveValue)
      bd += BigDecimal(Double.MaxValue)
      ca += Double.MinPositiveValue
      ca += Double.MaxValue //Double.MaxValue
      i += 1
    }
//    println(bd remainder BigDecimal(1.0))
//    println(ca.total remainder BigDecimal(1.0))
//    println(bd)
//    println(ca.total)
    assertEquals(bd, ca.total)

//    println(s"sum ${bd + bd}, ${(ca + bd).total}")
    assertEquals(bd + bd, (ca + bd).total)

//    println(s"product ${bd * bd}, ${(ca * ca).total}")
    assertEquals(bd * bd, (ca * ca).total)

//    println(s"quotient ${bd / bd}, ${(ca / ca).total}")
    assertEquals(bd / bd, (ca / ca).total)

//    println(s"quotient ${bd / BigDecimal(2.0)}, ${(ca / (ContinuousAccumulator() + BigDecimal(2.0))).total}")
    assertEquals(bd / BigDecimal(2.0), (ca / (ContinuousAccumulator() + BigDecimal(2.0))).total)

  }

}