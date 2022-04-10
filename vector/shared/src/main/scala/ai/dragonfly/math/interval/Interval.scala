package ai.dragonfly.math.interval

import ai.dragonfly.math.Random.defaultRandom
import ai.dragonfly.math.stats.probability.distributions.Sampleable

import math.Numeric.Implicits.infixNumericOps

object Interval {
  import ai.dragonfly.math.Random.*

  private inline def between(i:Interval[Int], r:scala.util.Random) = r.between(i.min, i.MAX)
  private inline def between(i:Interval[Long], r:scala.util.Random) = r.between(i.min, i.MAX)
  private inline def between(i:Interval[BigInt], r:scala.util.Random) = r.between(i.min, i.MAX)
  private inline def between(i:Interval[Float], r:scala.util.Random) = r.between(i.min, i.MAX)
  private inline def between(i:Interval[Double], r:scala.util.Random) = r.between(i.min, i.MAX)
  private inline def between(i:Interval[BigDecimal], r:scala.util.Random) = r.between(i.min, i.MAX)

  def randomFrom[N](ntrvl:Interval[N])(using r:scala.util.Random = defaultRandom): N = (ntrvl.min match {
    case i: Double => between(ntrvl.asInstanceOf[Interval[Double]], r)
    case i: Long => between(ntrvl.asInstanceOf[Interval[Long]], r)
    case i: Float => between(ntrvl.asInstanceOf[Interval[Float]], r)
    case i: Int => between(ntrvl.asInstanceOf[Interval[Int]], r)
    case i: BigDecimal => between(ntrvl.asInstanceOf[Interval[BigDecimal]], r)
    case i: BigInt => between(ntrvl.asInstanceOf[Interval[BigInt]], r)
  }).asInstanceOf[N]

  def rangeContains[N](ntrvl:Interval[N], x:Double): Boolean = ntrvl.min match {
    case d: Double => ntrvl.asInstanceOf[Interval[Double]].contains(x)
    case l: Long =>
      val i = ntrvl.asInstanceOf[Interval[Long]]
      apply[BigDecimal](i.code, BigDecimal(i.min), BigDecimal(i.MAX)).contains(BigDecimal(x))
    case f: Float =>
      val i = ntrvl.asInstanceOf[Interval[Float]]
      apply[Double](ntrvl.code, i.min.toDouble, i.MAX.toDouble).contains(x)
    case i: Int =>
      val i = ntrvl.asInstanceOf[Interval[Int]]
      apply[Double](ntrvl.code, i.min.toDouble, i.MAX.toDouble).contains(x)
    case bd: BigDecimal =>
      ntrvl.asInstanceOf[Interval[BigDecimal]].contains(BigDecimal(x))
    case bi: BigInt =>
      val i = ntrvl.asInstanceOf[Interval[BigInt]]
      apply[BigDecimal](ntrvl.code, BigDecimal(i.min), BigDecimal(i.MAX)).contains(BigDecimal(x))
  }

  def setContains[N](ntrvl:Interval[N], x:Double): Boolean = ntrvl.min match {
    case d: Double =>
      ntrvl.asInstanceOf[Interval[Double]].contains(x)
    case i: Int =>
      Domain.precisionLossSquared[Int](x, 0) == 0.0 && ntrvl.asInstanceOf[Interval[Int]].contains(x.toInt)
    case l: Long =>
      Domain.precisionLossSquared[Long](x, 0L) == 0.0 && ntrvl.asInstanceOf[Interval[Long]].contains(x.toLong)
    case f: Float =>
      Domain.precisionLossSquared[Float](x, 0f) == 0.0 && ntrvl.asInstanceOf[Interval[Float]].contains(x.toFloat)
    case bi: BigInt =>
      Domain.precisionLossSquared[BigInt](x, BigInt(0L)) == 0.0 && ntrvl.asInstanceOf[Interval[BigInt]].contains(BigDecimal(x).toBigInt)
    case bd: BigDecimal =>
      ntrvl.asInstanceOf[Interval[BigDecimal]].contains(BigDecimal(x))
  }

  def typeName[N](n:N):String = n match {
    case i:Int => "Int"
    case l:Long => "Long"
    case bi:BigInt => "BigInt"
    case f:Float => "Float"
    case d:Double => "Double"
    case bd:BigDecimal => "BigDecimal"
  }

  val OPEN:Int = 0x0
  val RIGHT_CLOSED:Int = 0x1
  val LEFT_CLOSED:Int = 0x1 << 1
  val CLOSED:Int = RIGHT_CLOSED | LEFT_CLOSED

  def `[]`[DOMAIN](min:DOMAIN, MAX:DOMAIN)(using `#`: Numeric[DOMAIN]):Interval[DOMAIN] = new Interval[DOMAIN](CLOSED, min, MAX)
  def `(]`[DOMAIN](min:DOMAIN, MAX:DOMAIN)(using `#`: Numeric[DOMAIN]):Interval[DOMAIN] = new Interval[DOMAIN](RIGHT_CLOSED, min, MAX)
  def `[)`[DOMAIN](min:DOMAIN, MAX:DOMAIN)(using `#`: Numeric[DOMAIN]):Interval[DOMAIN] = new Interval[DOMAIN](LEFT_CLOSED, min, MAX)
  def `()`[DOMAIN](min:DOMAIN, MAX:DOMAIN)(using `#`: Numeric[DOMAIN]):Interval[DOMAIN] = new Interval[DOMAIN](OPEN, min, MAX)

}

case class Interval[DOMAIN](code:Int, min:DOMAIN, MAX:DOMAIN)(using `#`: Numeric[DOMAIN]) extends Sampleable[DOMAIN] {
  import `#`.*

  lazy val norm:DOMAIN = `#`.minus(MAX, min)

  lazy val additiveIdentity:DOMAIN = `#`.zero
  lazy val multiplicativeIdentity:DOMAIN = `#`.one

  inline def leftClosed:Boolean = (code >> 1) > 0
  inline def rightClosed:Boolean = (code & 0x00000001) > 0

  def contains(x:DOMAIN):Boolean =  (if (leftClosed) x >= min else x > min) && (if (rightClosed) x <= MAX else x < MAX)

  def rangeContains(x: Double): Boolean = Interval.rangeContains(this, x)

  def setContains(x: Double): Boolean = Interval.setContains(this, x)

  import ai.dragonfly.math.Random.*


  def random(r0:scala.util.Random = defaultRandom): DOMAIN = {
    given r:scala.util.Random = r0
    var rv:DOMAIN = Interval.randomFrom(this).asInstanceOf[DOMAIN]
    while (!this.contains(rv)) rv = Interval.randomFrom(this).asInstanceOf[DOMAIN]
    rv
  }

  override def toString:String = s"${if(leftClosed) "[" else "("} $min, $MAX ${if(rightClosed) "]" else ")" }${Interval.typeName(min)}"

}
