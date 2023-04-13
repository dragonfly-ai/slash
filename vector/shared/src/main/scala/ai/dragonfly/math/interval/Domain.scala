package ai.dragonfly.math.interval

import Interval.*


object Domain {

  val ℕ_Int: Domain[Int] = Domain[Int](`[]`(1, Int.MaxValue))
  val ℕ_Long: Domain[Long] = Domain[Long](`[]`(1L, Long.MaxValue))

  val `ℕ₀_Int`: Domain[Int] = Domain[Int](`[]`(0, Int.MaxValue))
  val `ℕ₀_Long`: Domain[Long] = Domain[Long](`[]`(0, Long.MaxValue))

  val ℤ_Int: Domain[Int] = Domain[Int](`[]`(Int.MinValue, Int.MaxValue))
  val ℤ_Long: Domain[Long] = Domain[Long](`[]`(Long.MinValue, Long.MaxValue))

  val ℝ_Float: Domain[Float] = Domain[Float](`[]`(Float.MinValue, Float.MaxValue))
  val ℝ_Double: Domain[Double] = Domain[Double](`[]`(Double.MinValue, Double.MaxValue))

  val `ℝ+_Float`: Domain[Float] = Domain[Float](`(]`(0f, Float.MaxValue))
  val `ℝ+_Double`: Domain[Double] = Domain[Double](`(]`(0.0, Double.MaxValue))

  val `ℝ[0,1]_Float`: Domain[Float] = Domain[Float](`[]`(0f, 1f))
  val `ℝ[0,1]_Double`: Domain[Double] = Domain[Double](`[]`(0.0, 1.0))

  val `ℝ[-1,1]_Float`: Domain[Float] = Domain[Float](`[]`(-1f, 1f))
  val `ℝ[-1,1]_Double`: Domain[Double] = Domain[Double](`[]`(-1.0, 1.0))

  trait Overflows[A, Z]:
    def apply(x:A):Boolean

  given Overflows[Any, BigInt] with
    override inline def apply(x: Any): Boolean = false

  given Overflows[Any, BigDecimal] with
    override inline def apply(x: Any): Boolean = false

  // Byte
  given Overflows[Byte, Short | Int | Float | Double | Long | BigDecimal | BigInt] with
    override inline def apply(x: Byte): Boolean = false

  // Short
  given Overflows[Short, Byte] with
    override inline def apply(x: Short): Boolean = x > Byte.MaxValue.toShort | x < Byte.MinValue.toShort

  given Overflows[Short, Short] with
    override inline def apply(x: Short): Boolean = false

  given Overflows[Short, Int] with
    override inline def apply(x: Short): Boolean = false

  given Overflows[Short, Long] with
    override inline def apply(x: Short): Boolean = false

  given Overflows[Short, Float] with
    override inline def apply(x: Short): Boolean = false

  given Overflows[Short, Double] with
    override inline def apply(x: Short): Boolean = false


  // Int
  given Overflows[Int, Byte] with
    override inline def apply(x: Int): Boolean = x > Byte.MaxValue.toInt | x < Byte.MinValue.toInt

  given Overflows[Int, Short] with
    override inline def apply(x: Int): Boolean = x > Short.MaxValue.toInt | x < Short.MinValue.toInt // 2147483647

  given Overflows[Int, Int] with
    override inline def apply(x: Int): Boolean = false

  given Overflows[Int, Long] with
    override inline def apply(x: Int): Boolean = false

  given Overflows[Int, Float] with
    override inline def apply(x: Int): Boolean = false

  given Overflows[Int, Double] with
    override inline def apply(x: Int): Boolean = false

  // Long
  given Overflows[Long, Byte] with
    override inline def apply(x: Long): Boolean = x > Byte.MaxValue.toLong | x < Byte.MinValue.toLong

  given Overflows[Long, Short] with
    override inline def apply(x: Long): Boolean = x > Short.MaxValue.toLong | x < Short.MinValue.toLong

  given Overflows[Long, Int] with
    override inline def apply(x: Long): Boolean = x > Int.MaxValue.toLong | x < Int.MinValue.toLong

  given Overflows[Long, Long] with
    override inline def apply(x: Long): Boolean = false

  given Overflows[Long, Float] with
    override inline def apply(x: Long): Boolean = false

  given Overflows[Long, Double] with
    override inline def apply(x: Long): Boolean = false

  // Float
  given Overflows[Float, Byte] with
    override inline def apply(x: Float): Boolean = x > Byte.MaxValue.toFloat | x < Byte.MinValue.toFloat

  given Overflows[Float, Short] with
    override inline def apply(x: Float): Boolean = x > Short.MaxValue.toFloat | x < Short.MinValue.toFloat // 2147483647

  given Overflows[Float, Int] with
    override inline def apply(x: Float): Boolean = x > Int.MaxValue.toFloat | x < Int.MinValue.toFloat

  given Overflows[Float, Long] with
    override inline def apply(x: Float): Boolean = x > Long.MaxValue.toFloat | x < Long.MinValue.toFloat

  given Overflows[Float, Float] with
    override inline def apply(x: Float): Boolean = false

  given Overflows[Float, Double] with
    override inline def apply(x: Float): Boolean = false

  // Double
  given Overflows[Double, Byte] with
    override inline def apply(x: Double): Boolean = x > Byte.MaxValue.toDouble | x < Byte.MinValue.toDouble

  given Overflows[Double, Short] with
    override inline def apply(x: Double): Boolean = x > Short.MaxValue.toDouble | x < Short.MinValue.toDouble // 2147483647

  given Overflows[Double, Int] with
    override inline def apply(x: Double): Boolean = x > Int.MaxValue.toDouble | x < Int.MinValue.toDouble

  given Overflows[Double, Long] with
    override inline def apply(x: Double): Boolean = x > Long.MaxValue.toDouble | x < Long.MinValue.toDouble

  given Overflows[Double, Float] with
    override inline def apply(x: Double): Boolean = x > Float.MaxValue.toFloat | x < Float.MinValue.toFloat

  given Overflows[Double, Double] with
    override inline def apply(x: Double): Boolean = false


  import ai.dragonfly.math.squareInPlace
  // convert to type class ?
  def precisionLossSquared[T](d:Double, t:T):Double = squareInPlace(
    d - (
      t match {
        case t: Byte => (d.toByte).toDouble
        case t: Short => (d.toShort).toDouble
        case t: Int => (d.toInt).toDouble
        case t: Long => (d.toLong).toDouble
        case t: BigInt => BigInt(d.toLong).toDouble
        case t: Float => (d.toFloat).toDouble
        case t: Double => d
        case t: BigDecimal => BigDecimal(d).toDouble
      }
    )
  )

}

case class Domain[DOMAIN](interval:Interval[DOMAIN])(using `#`: Numeric[DOMAIN]) {
  val `0`:DOMAIN = `#`.zero // Additive Identity
  val `1`:DOMAIN = `#`.one // Multiplicative Identity
  def zero:DOMAIN = `0`
  def one:DOMAIN = `1`
  def min:DOMAIN = interval.min
  def MAX:DOMAIN = interval.MAX
}
