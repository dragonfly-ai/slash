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

  import scala.quoted.*

  /**
   * Compute a logarithm of the given base.
   * Because Logₓ(y) = Math.log10(y) / Math.log10(x), this method computes the denominator at compile time.
   * For example, to compute log base 2 of 64: {{{ log[2](64) }}}
   * To compute log base π of 42: {{{ log[3.141592653589793](42) }}}
   * @param y the input to the logarithm operator.
   * @tparam BASE the base of the logarithm.
   * @return logₓ(y) where x = base and y = d.
   */
  inline def log[BASE <: Double | Int](y:Double): Double = ${ logImpl[BASE]('{y}) }

  private def logImpl[BASE: Type](x:Expr[Double])(using Quotes): Expr[Double] = {
    import quotes.reflect.*
    val base:Double = TypeRepr.of[BASE] match {
      case ConstantType(c:Constant) =>
        c.value match {
          case i:Int => i.toDouble
          case d:Double => d
          case a => throw Exception(s"Expected log[Double|Int] but found log[$a].")
        }
      //case TermRef(a, b) => println(s"found ${a} ${b}"); 0.0
      case notaconstant =>
        throw Exception(s"Expected log[Double|Int] but found log[${notaconstant}].")
    }

    '{ Math.log10(${x}) / ${ Expr(Math.log10(base)) } }
  }

  /**
   * This class can yield drastic performance improvements in cases when the base of an oft-repeated logarithm operation
   * can't be known at compile time, or can't be expressed as a constant.
   * @param base the base of the logarithm operator.
   */
  case class Log(val base: Double) {
    private val `log10(base)`: Double = Math.log10(base)

    /**
     * Computes logarithm with a base defined in the constructor of the class of the operand specified by the x parameter.
     * @param x the operand to this logarithm operator.
     * @return
     */
    inline def apply(x:Double): Double = Math.log10(x) / `log10(base)`
  }

  inline def degreesToRadians(degrees: Double):Double = degrees * 0.017453292519943295
  inline def radiansToDegrees(radians: Double):Double = radians * 57.29577951308232
}