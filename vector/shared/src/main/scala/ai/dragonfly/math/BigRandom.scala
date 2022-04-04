package ai.dragonfly.math

import java.math.MathContext
import scala.util.Random


object BigRandom {

  lazy val defaultRandom:Random = new Random()

  private val log2 = Math.log10(2.0)

  /**
   * Computes number of bits needed to represent an n digit positive integer.
   *
   * @param n number of base ten digits.
   * @return number of bits required to represent an n digit positive integer.
   */
  private def bitCount(n: Int) = (n / log2).toInt

  /**
   * Computes the number of base ten digits required to represent an n bit positive integer.
   *
   * @param n number of bits.
   * @return number of base ten digits required to represent an n bit positive integer.
   */
  private def digitCount(n: Int) = (n * log2).toInt

  
  // Static Methods for generating Random BigInt values:

  def nextBigInt(precision: Int): BigInt = nextBigInt(precision, defaultRandom)

  def nextBigInt(precision: Int, r: Random) = BigInt(bitCount(precision), r)

  def nextBigInt(norm: BigInt): BigInt = nextBigInt(norm, defaultRandom)

  def nextBigInt(norm: BigInt, r: Random): BigInt = {
    val bdNorm = BigDecimal(norm)
    val precision = bdNorm.precision - bdNorm.scale
    (bdNorm(new MathContext(precision + 1)) * nextBigDecimal(precision, r)).toBigInt
  }

  def between(min: BigInt, MAX: BigInt): BigInt = between(min, MAX, defaultRandom)

  def between(min: BigInt, MAX: BigInt, r: Random): BigInt = min + nextBigInt(MAX - min, r)


  // Static Methods for generating Random BigDecimal values:
  def nextBigDecimal(scale: Int): BigDecimal = nextBigDecimal(scale, defaultRandom)

  def nextBigDecimal(scale: Int, r: Random): BigDecimal = {
    val bd = BigDecimal( nextBigInt(scale, r) ) // convert BigInt to a BigDecimal
    BigDecimal(bd.bigDecimal.movePointLeft(bd.precision)) // move the decimal point all the way to the left
  }

  def nextBigDecimal(norm: BigDecimal, scale: Int): BigDecimal = nextBigDecimal(norm, scale, defaultRandom)

  def nextBigDecimal(norm: BigDecimal, scale: Int, r: Random): BigDecimal = {
    norm * nextBigDecimal(scale, r)(new MathContext((norm.precision - norm.scale) + scale))
  }

  def between(min: BigDecimal, MAX: BigDecimal): BigDecimal = between(min, MAX, defaultRandom)

  def between(min: BigDecimal, MAX: BigDecimal, r: Random): BigDecimal = {
    min + nextBigDecimal(MAX - min, Math.max(min.scale, MAX.scale), r)
  }

}