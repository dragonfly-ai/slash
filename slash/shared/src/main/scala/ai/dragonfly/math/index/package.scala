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

package ai.dragonfly

import narr.NArray
import ai.dragonfly.math.vector.VectorSpace

package object idx {

  opaque type Index[N <: Int] = NArray[Boolean]

  object Index {
    def none(num: Int): Index[num.type] = NArray.fill(num)(false)
    def trues(num: Int) : Index[num.type] = NArray.fill(num)(true)
    def none[N <: Int](using ValueOf[N]) : Index[N] = NArray.fill[Boolean](valueOf[N])(false)
    def all[N <: Int](using ValueOf[N]) : Index[N] = NArray.fill[Boolean](valueOf[N])(true)

  }


  extension[N <: Int] (thisVector: Index[N]) {
      inline def apply(idx: Int): Boolean = thisVector(idx)
      inline def at(idx: Int): Boolean = thisVector(idx)
      inline def update(idx: Int, value: Boolean): Unit = thisVector(idx) = value
      inline def changeAt(idx: Int, value: Boolean): Unit = thisVector(idx) = value
      inline def dimension: Int = thisVector.length
      inline def countTrue : Int =
        var sum = 0
        for(i <- 0 until dimension) {
          if(thisVector(i)) sum = sum + 1
        }
        sum
      end countTrue

      inline def vectorSpace: VectorSpace[?] = VectorSpace.apply(dimension)

      inline def &&(thatIdx: Index[N]): Index[N] = {
        val result:Index[N] = new NArray[Boolean](dimension)
        for(i <- 0 until dimension) {
          result(i) = thisVector(i) && thatIdx(i)
        }
        result
      }

      inline def ||(thatIdx: Index[N]): Index[N] = {
        val result:Index[N] = new NArray[Boolean](dimension)
        for(i <- 0 until dimension) {
          result(i) = thisVector(i) || thatIdx(i)
        }
        result
      }

      def copy:Index[N] = {
        val copyOfThisVector:Index[N] = new NArray[Boolean](dimension)
        var i = 0
        while (i < dimension) {
          copyOfThisVector(i) = thisVector(i)
          i = i + 1
        }
        copyOfThisVector
      }
  }

}
