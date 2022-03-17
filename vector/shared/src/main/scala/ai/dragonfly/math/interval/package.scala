package ai.dragonfly.math

import ai.dragonfly.math.stats.probability.distributions.Sampleable
import math.Numeric.Implicits.infixNumericOps

package object interval {

  trait Interval[DOMAIN](using `#`: Numeric[DOMAIN]) { // extends Sampleable[DOMAIN] {

    val min:DOMAIN
    val MAX:DOMAIN

    protected val min_bd:BigDecimal = min match {
      case minL:Long => BigDecimal(minL)
      case minI:Int => BigDecimal(minI)
      case minD:Double => BigDecimal(minD)
      case minF:Float => BigDecimal(minF)
    }
    protected val MAX_BD:BigDecimal = MAX match {
      case lMAX:Long => BigDecimal(lMAX)
      case iMAX:Int => BigDecimal(iMAX)
      case dMAX:Double => BigDecimal(dMAX)
      case fMAX:Float => BigDecimal(fMAX)
    }

//    override def random(): DOMAIN = {
//      val r = ai.dragonfly.math.Random.For[DOMAIN]
//      this.min + r.apply(MAX - this.min)
//    }
//    val additiveIdentity:DOMAIN = `#`.zero
//    val multiplicativeIdentity:DOMAIN = `#`.one

    def contains(x:DOMAIN):Boolean
    def contains(x:BigDecimal):Boolean
    inline def contains(x: Int): Boolean = contains(BigDecimal(x))
    inline def contains(x: Long): Boolean = contains(BigDecimal(x))
    inline def contains(x: Float): Boolean = contains(BigDecimal(x))
    inline def contains(x: Double): Boolean = contains(BigDecimal(x))
  }

  case class `[]`[DOMAIN]( override val min:DOMAIN, override val MAX:DOMAIN )(using `#`: Numeric[DOMAIN]) extends Interval[DOMAIN] {
    import `#`._
    override inline def contains(x: DOMAIN): Boolean = x >= min && x <= MAX
    override inline def contains(x: BigDecimal): Boolean = x >= min_bd && x <= MAX_BD
  }
  type Closed = `[]`.type

  case class `()`[DOMAIN]( override val min:DOMAIN, override val MAX:DOMAIN )(using `#`: Numeric[DOMAIN]) extends Interval[DOMAIN] {
    import `#`._
    override inline def contains(x: DOMAIN): Boolean = x > min && x < MAX
    override inline def contains(x: BigDecimal): Boolean = x > min_bd && x < MAX_BD
  }
  type Open = `()`.type

  case class `[)`[DOMAIN]( override val min:DOMAIN, override val MAX:DOMAIN )(using `#`: Numeric[DOMAIN]) extends Interval[DOMAIN] {
    import `#`._
    override inline def contains(x: DOMAIN): Boolean = x >= min && x < MAX
    override inline def contains(x: BigDecimal): Boolean = x >= min_bd && x < MAX_BD
  }
  type ClosedOpen = `[)`.type

  case class `(]`[DOMAIN]( override val min:DOMAIN, override val MAX:DOMAIN )(using `#`: Numeric[DOMAIN]) extends Interval[DOMAIN] {
    import `#`._
    override inline def contains(x: DOMAIN): Boolean = x > min && x <= MAX
    override inline def contains(x: BigDecimal): Boolean = x > min_bd && x <= MAX_BD
  }
  type OpenClosed = `(]`.type
}
