package ai.dragonfly.math

import ai.dragonfly.math.stats.probability.distributions.Sampleable

import math.Numeric.Implicits.infixNumericOps

import scala.util.Random

package object interval {

  lazy val defaultRandom:Random = new Random()

  trait Interval[DOMAIN](using `#`: Numeric[DOMAIN]) extends Sampleable[DOMAIN] {

    val min:DOMAIN
    val MAX:DOMAIN

    def toBigDecimal(n:DOMAIN):BigDecimal = n match {
      case b:Byte => BigDecimal(b)
      case s:Short => BigDecimal(s)
      case i:Int => BigDecimal(i)
      case l:Long => BigDecimal(l)
      case bi:BigInt => BigDecimal(bi)
      case f:Float => BigDecimal(f)
      case d:Double => BigDecimal(d)
      case d:BigDecimal => d
    }

    protected lazy val min_bd:BigDecimal = toBigDecimal(min)
    protected lazy val MAX_BD:BigDecimal = toBigDecimal(MAX)

    val additiveIdentity:DOMAIN = `#`.zero
    val multiplicativeIdentity:DOMAIN = `#`.one

    def contains(x:DOMAIN):Boolean
    def contains(bd:BigDecimal):Boolean

    def random():DOMAIN = this.random(defaultRandom)()

    def random(r:Random):() => DOMAIN = {
      () => (this.min match {
        case mn:Byte => () => r.between(mn.toInt, MAX.asInstanceOf[Byte].toInt).toByte
        case mn: Short => () => r.between(mn.toInt, MAX.asInstanceOf[Short].toInt).toShort
        case mn: Int => () => r.between(mn, MAX.asInstanceOf[Int])
        case mn: Long => () => r.between(mn, MAX.asInstanceOf[Long])
        case mn: BigInt => () => BigRandom.between(mn, MAX.asInstanceOf[BigInt], r)
        case mn: Float => () => r.between(mn, MAX.asInstanceOf[Float])
        case mn: Double => () => r.between(mn, MAX.asInstanceOf[Double])
        case mn: BigDecimal => () => BigRandom.between(mn, MAX.asInstanceOf[BigDecimal], r)
      })().asInstanceOf[DOMAIN]
    }

    val norm: DOMAIN = MAX - min
    lazy val bdNorm: BigDecimal = MAX_BD - min_bd

    val typeName:String = min match {
      case b:Byte => "Byte"
      case s:Short => "Short"
      case i:Int => "Int"
      case l:Long => "Long"
      case bi:BigInt => "BigInt"
      case f:Float => "Float"
      case d:Double => "Double"
      case bd:BigDecimal => "BigDecimal"
    }

    override def toString:String = s"${if(contains(min)) "[" else "("} $min, $MAX ${if(contains(MAX)) "]" else ")" }$typeName"

  }

  case class `[]`[DOMAIN]( override val min:DOMAIN, override val MAX:DOMAIN )(using `#`: Numeric[DOMAIN]) extends Interval[DOMAIN] {
    import `#`._
    override inline def contains(x: DOMAIN): Boolean = x >= min && x <= MAX
    override inline def contains(bd: BigDecimal): Boolean = bd >= min_bd && bd <= MAX_BD
  }
  type Closed = `[]`.type

  case class `()`[DOMAIN]( override val min:DOMAIN, override val MAX:DOMAIN )(using `#`: Numeric[DOMAIN]) extends Interval[DOMAIN] {
    import `#`._
    override inline def contains(x: DOMAIN): Boolean = x > min && x < MAX
    override inline def contains(bd: BigDecimal): Boolean = bd > min_bd && bd < MAX_BD
  }
  type Open = `()`.type

  case class `[)`[DOMAIN]( override val min:DOMAIN, override val MAX:DOMAIN )(using `#`: Numeric[DOMAIN]) extends Interval[DOMAIN] {
    import `#`._
    override inline def contains(x: DOMAIN): Boolean = x >= min && x < MAX
    override inline def contains(bd: BigDecimal): Boolean = bd >= min_bd && bd < MAX_BD
  }
  type ClosedOpen = `[)`.type

  case class `(]`[DOMAIN]( override val min:DOMAIN, override val MAX:DOMAIN )(using `#`: Numeric[DOMAIN]) extends Interval[DOMAIN] {
    import `#`._
    override inline def contains(x: DOMAIN): Boolean = x > min && x <= MAX
    override inline def contains(bd: BigDecimal): Boolean = bd > min_bd && bd <= MAX_BD
  }
  type OpenClosed = `(]`.type
}
