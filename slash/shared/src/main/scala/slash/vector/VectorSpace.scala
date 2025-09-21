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

package slash.vector

import slash.*
import narr.NArray

object VectorSpace {
  def apply(dimension:Int):VectorSpace[dimension.type] = new VectorSpace[dimension.type]
}


class VectorSpace[N0 <: Int](using dt: ValueOf[N0]) {

  val dimension:Int = dt.value
  dimensionCheck(dimension, dt.value)

  opaque type N <: Int = N0
  given n: ValueOf[N] = dt

  inline def apply(a: NArray[Double]): Vec[N] = {
    dimensionCheck(a.length, dimension)
    a.asInstanceOf[Vec[N]]
  }

  inline def apply(d: Double*): Vec[N] = Vec.apply[N](d: _*)

  def fromTuple(t:Tuple):Vec[N] = {
    dimensionCheck(t.productArity, dimension)
    val itr:Iterator[Any] = t.productIterator
    val v:NArray[Double] = new NArray[Double](dimension)
    var i:Int = 0
    while (itr.hasNext) {
      v(i) = itr.next().asInstanceOf[Double]
      i += 1
    }
    apply(v)
  }

  inline def tabulate(f: (i: Int) => Double): Vec[N] = apply(NArray.tabulate[Double](dimension)(f))

  inline def fill(d: Double): Vec[N] = apply(NArray.fill[Double](dimension)(d))

  inline def zeros: Vec[N] = apply(NArray.fill[Double](dimension)(0.0))

  inline def ones: Vec[N] = apply(NArray.fill[Double](dimension)(1.0))

  import slash.Random.*

  inline def random(
    min: Double = 0.0,
    MAX: Double = 1.0,
    r: scala.util.Random = slash.Random.defaultRandom
  ): Vec[N] = r.nextVec[N](min, MAX)

  def between[N <: Int](
    min: Vec[N],
    MAX: Vec[N],
    r: scala.util.Random = slash.Random.defaultRandom
  )(using ValueOf[N]): Vec[N] = r.between[N](min, MAX)

}
