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

import ai.dragonfly.math.Gamma
import ai.dragonfly.math.vector.*

package object math {

  inline def squareInPlace(b:Byte): Byte = (b*b).toByte
  inline def squareInPlace(s:Short): Short = (s*s).toShort
  inline def squareInPlace(i:Int): Int = i*i
  inline def squareInPlace(l:Long): Long = l*l
  inline def squareInPlace(bi:BigInt): BigInt = bi*bi
  inline def squareInPlace(f:Float): Float = f*f
  inline def squareInPlace(d:Double): Double = d*d
  inline def squareInPlace(bd:BigDecimal): BigDecimal = bd*bd

  inline def cubeInPlace(b:Byte): Byte = (b*b*b).toByte
  inline def cubeInPlace(s:Short): Short = (s*s*s).toShort
  inline def cubeInPlace(i:Int): Int = i*i*i
  inline def cubeInPlace(l:Long): Long = l*l*l
  inline def cubeInPlace(bi:BigInt): BigInt = bi*bi*bi
  inline def cubeInPlace(f:Float): Float = f*f*f
  inline def cubeInPlace(d:Double): Double = d*d*d
  inline def cubeInPlace(bd:BigDecimal): BigDecimal = bd*bd*bd


  def Γ(x:Double):Double = Math.exp(lnGamma(x))
  def gamma(x:Double):Double = Γ(x)
  def lnGamma(x:Double):Double = Gamma.lnGamma(x)

  def B(α:Double, β:Double):Double = ( Γ(α) * Γ(β) ) / Γ(α + β)
  def beta(α:Double, β:Double):Double = B(α, β)

  inline def ln(x:Double):Double = Math.log(x)
  inline def degreesToRadians(degrees: Double):Double = degrees * 0.017453292519943295
  inline def radiansToDegrees(radians: Double):Double = radians * 57.29577951308232
}