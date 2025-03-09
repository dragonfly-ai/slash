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

package object slash {

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

  inline def `√`(d:Double):Double = Math.sqrt(d)
  inline def `∛`(d:Double):Double = Math.pow(d, Constant.`¹/₃`)
  inline def `∜`(d:Double):Double = Math.pow(d, Constant.`¹/₄`)

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
        throw Exception(s"Expected log[Double|Int] but found log[$notaconstant].")
    }

    '{ Math.log10(${x}) / ${ Expr(Math.log10(base)) } }
  }

  /**
   * This class can yield drastic performance improvements in cases when the base of an oft-repeated logarithm operation
   * can't be known at compile time, or can't be expressed as a constant.
   * @param base the base of the logarithm operator.
   */
  case class Log(base: Double) {
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

  private lazy val bigDecimalInfinitesimal:BigDecimal = BigDecimal(1L, Int.MaxValue)

  def nextUp[N](x: N): N = (x match {
    case xt: Byte => if (xt < Byte.MaxValue) (xt + 1).toByte else Byte.MaxValue
    case xt: Short => if (xt < Short.MaxValue) (xt + 1).toShort else Short.MaxValue
    case xt: Int => if (xt < Int.MaxValue) xt + 1 else Int.MaxValue
    case xt: Long => if (xt < Long.MaxValue) xt + 1L else Long.MaxValue
    case xt: BigInt => xt + 1
    case xt: Float => Math.nextUp(xt)
    case xt: Double => Math.nextUp(xt)
    case xt: BigDecimal => xt + bigDecimalInfinitesimal
  }).asInstanceOf[N]

  def nextDown[N](x: N): N = (x match {
    case xt: Byte => if (xt > Byte.MinValue) (xt - 1).toByte else Byte.MinValue
    case xt: Short => if (xt > Short.MinValue) (xt - 1).toShort else Short.MinValue
    case xt: Int => if (xt > Int.MinValue) xt - 1 else Int.MinValue
    case xt: Long => if (xt > Long.MinValue) xt - 1L else Long.MinValue
    case xt: BigInt => xt - 1
    case xt: Float => Math.nextDown(xt)
    case xt: Double => Math.nextDown(xt)
    case xt: BigDecimal => xt - bigDecimalInfinitesimal
  }).asInstanceOf[N]


  extension (b: Byte) {
    inline def nextUp: Byte = if (b < Byte.MaxValue) (b + 1).toByte else Byte.MaxValue
    inline def nextDown: Byte = if (b > Byte.MinValue) (b - 1).toByte else Byte.MinValue
    inline def <(x: BigInt): Boolean = b.toInt < x
    inline def >(x: BigInt): Boolean = b.toInt > x
    inline def <=(x: BigInt): Boolean = b.toInt <= x
    inline def >=(x: BigInt): Boolean = b.toInt >= x

    inline def <(x: BigDecimal): Boolean = b.toFloat < x
    inline def >(x: BigDecimal): Boolean = b.toFloat > x
    inline def <=(x: BigDecimal): Boolean = b.toFloat <= x
    inline def >=(x: BigDecimal): Boolean = b.toFloat >= x
  }

  extension (b: Short) {
    inline def nextUp: Short = if (b < Short.MaxValue) (b + 1).toShort else Short.MaxValue
    inline def nextDown: Short = if (b > Short.MinValue) (b - 1).toShort else Short.MinValue
    inline def <(x: BigInt): Boolean = b.toInt < x
    inline def >(x: BigInt): Boolean = b.toInt > x
    inline def <=(x: BigInt): Boolean = b.toInt <= x
    inline def >=(x: BigInt): Boolean = b.toInt >= x

    inline def <(x: BigDecimal): Boolean = b.toFloat < x
    inline def >(x: BigDecimal): Boolean = b.toFloat > x
    inline def <=(x: BigDecimal): Boolean = b.toFloat <= x
    inline def >=(x: BigDecimal): Boolean = b.toFloat >= x
  }

  extension (i:Int) {
    inline def nextUp:Int = if (i < Int.MaxValue) i + 1 else Int.MaxValue
    inline def nextDown:Int = if (i > Int.MinValue) i - 1 else Int.MinValue

  }

  extension (l: Long) {
    inline def nextUp: Long = if (l < Long.MaxValue) l + 1L else Long.MaxValue
    inline def nextDown: Long = if (l > Long.MinValue) l - 1L else Long.MinValue

  }

  extension (f: Float) {
    inline def nextUp: Float = Math.nextUp(f)
    inline def nextDown: Float = Math.nextDown(f)
    inline def <(x: BigInt): Boolean = BigDecimal(f) < BigDecimal(x)
    inline def >(x: BigInt): Boolean = BigDecimal(f) > BigDecimal(x)
    inline def <=(x: BigInt): Boolean = BigDecimal(f) <= BigDecimal(x)
    inline def >=(x: BigInt): Boolean = BigDecimal(f) >= BigDecimal(x)

    inline def <(x: BigDecimal): Boolean = BigDecimal(f) < x
    inline def >(x: BigDecimal): Boolean = BigDecimal(f) > x
    inline def <=(x: BigDecimal): Boolean = BigDecimal(f) <= x
    inline def >=(x: BigDecimal): Boolean = BigDecimal(f) >= x
  }



  extension (d: Double) {
    inline def nextUp: Double = Math.nextUp(d)
    inline def nextDown: Double = Math.nextDown(d)
    inline def <(x: BigInt): Boolean = BigDecimal(d) < BigDecimal(x)
    inline def >(x: BigInt): Boolean = BigDecimal(d) > BigDecimal(x)
    inline def <=(x: BigInt): Boolean = BigDecimal(d) <= BigDecimal(x)
    inline def >=(x: BigInt): Boolean = BigDecimal(d) >= BigDecimal(x)
  }


  /*
    BigInt needs: byte, short, float, double, BigDecimal
   */

  extension (bi: BigInt) {
    inline def nextUp: BigInt = bi + 1
    inline def nextDown: BigInt = bi - 1
    inline def <(x: Byte): Boolean = bi < x.toInt
    inline def >(x: Byte): Boolean = bi > x.toInt
    inline def <=(x: Byte): Boolean = bi <= x.toInt
    inline def >=(x: Byte): Boolean = bi >= x.toInt

    inline def <(x: Short): Boolean = bi < x.toInt
    inline def >(x: Short): Boolean = bi > x.toInt
    inline def <=(x: Short): Boolean = bi <= x.toInt
    inline def >=(x: Short): Boolean = bi >= x.toInt

    inline def <(x: Float): Boolean = BigDecimal(bi) < BigDecimal(x.toDouble)
    inline def >(x: Float): Boolean = BigDecimal(bi) > BigDecimal(x.toDouble)
    inline def <=(x: Float): Boolean = BigDecimal(bi) <= BigDecimal(x.toDouble)
    inline def >=(x: Float): Boolean = BigDecimal(bi) >= BigDecimal(x.toDouble)

    inline def <(x: Double): Boolean = BigDecimal(bi) < BigDecimal(x)
    inline def >(x: Double): Boolean = BigDecimal(bi) > BigDecimal(x)
    inline def <=(x: Double): Boolean = BigDecimal(bi) <= BigDecimal(x)
    inline def >=(x: Double): Boolean = BigDecimal(bi) >= BigDecimal(x)

    inline def <(x: BigDecimal): Boolean = BigDecimal(bi) < x
    inline def >(x: BigDecimal): Boolean = BigDecimal(bi) > x
    inline def <=(x: BigDecimal): Boolean = BigDecimal(bi) <= x
    inline def >=(x: BigDecimal): Boolean = BigDecimal(bi) >= x
  }


  /*
    BigDecimal needs: byte, short, BigInt, float
   */

  extension (bd: BigDecimal) {
    inline def nextUp: BigDecimal = bd + bigDecimalInfinitesimal
    inline def nextDown: BigDecimal = bd - bigDecimalInfinitesimal
    inline def <(x: Byte): Boolean = bd < x.toInt
    inline def >(x: Byte): Boolean = bd > x.toInt
    inline def <=(x: Byte): Boolean = bd <= x.toInt
    inline def >=(x: Byte): Boolean = bd >= x.toInt

    inline def <(x: Short): Boolean = bd < x.toInt
    inline def >(x: Short): Boolean = bd > x.toInt
    inline def <=(x: Short): Boolean = bd <= x.toInt
    inline def >=(x: Short): Boolean = bd >= x.toInt

    inline def <(x: Float): Boolean = bd < BigDecimal(x.toDouble)
    inline def >(x: Float): Boolean = bd > BigDecimal(x.toDouble)
    inline def <=(x: Float): Boolean = bd <= BigDecimal(x.toDouble)
    inline def >=(x: Float): Boolean = bd >= BigDecimal(x.toDouble)

    inline def <(x: Double): Boolean = bd < BigDecimal(x)
    inline def >(x: Double): Boolean = bd > BigDecimal(x)
    inline def <=(x: Double): Boolean = bd <= BigDecimal(x)
    inline def >=(x: Double): Boolean = bd >= BigDecimal(x)

    inline def <(x: BigInt): Boolean = bd < BigDecimal(x)
    inline def >(x: BigInt): Boolean = bd > BigDecimal(x)
    inline def <=(x: BigInt): Boolean = bd <= BigDecimal(x)
    inline def >=(x: BigInt): Boolean = bd >= BigDecimal(x)
  }
}
