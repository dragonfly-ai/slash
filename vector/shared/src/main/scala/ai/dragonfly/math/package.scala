package ai.dragonfly

import ai.dragonfly.math.Gamma
import ai.dragonfly.math.example.*
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

  def Γ(x:Double):Double = Math.exp(logGamma(x))
  def gamma(x:Double):Double = Γ(x)
  def logGamma(x:Double):Double = Gamma.logGamma(x)

  def B(α:Double, β:Double):Double = ( Γ(α) * Γ(β) ) / Γ(α + β)
  def beta(α:Double, β:Double):Double = B(α, β)


}