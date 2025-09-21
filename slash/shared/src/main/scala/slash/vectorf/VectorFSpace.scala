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

package slash.vectorf

import narr.NArray
import vectorf.VecF

import slash.*

object VectorFSpace {
  def apply(dimension:Int):VectorFSpace[dimension.type] = new VectorFSpace[dimension.type]
}

class VectorFSpace[N0 <: Int](using dt: ValueOf[N0]) {

  val dimension:Int = dt.value
  dimensionCheck(dimension, dt.value)

  opaque type N <: Int = N0
  given n: ValueOf[N] = dt

  inline def apply(a: NArray[Float]): VecF[N] = {
    dimensionCheck(a.length, dimension)
    a.asInstanceOf[VecF[N]]
  }

  inline def apply(d: Float*): VecF[N] = VecF.apply[N](d: _*)

  def fromTuple(t:Tuple):VecF[N] = {
    dimensionCheck(t.productArity, dimension)
    val itr:Iterator[Any] = t.productIterator
    val v:NArray[Float] = new NArray[Float](dimension)
    var i:Int = 0
    while (itr.hasNext) {
      v(i) = itr.next().asInstanceOf[Float]
      i += 1
    }
    apply(v)
  }

  inline def tabulate(f: (i: Int) => Float): VecF[N] = apply(NArray.tabulate[Float](dimension)(f))

  inline def fill(f: Float): VecF[N] = apply(NArray.fill[Float](dimension)(f))

  inline def zeros: VecF[N] = apply(NArray.fill[Float](dimension)(0.0f))

  inline def ones: VecF[N] = apply(NArray.fill[Float](dimension)(1.0f))

  import slash.Random.*

  inline def random(
    min: Float = 0.0f,
    MAX: Float = 1.0f,
    r: scala.util.Random = slash.Random.defaultRandom
  ): VecF[N] = r.nextVecF[N](min, MAX)

  def between[N <: Int](
    min: VecF[N],
    MAX: VecF[N],
    r: scala.util.Random = slash.Random.defaultRandom
  )(using ValueOf[N]): VecF[N] = r.betweenF[N](min, MAX)

}
