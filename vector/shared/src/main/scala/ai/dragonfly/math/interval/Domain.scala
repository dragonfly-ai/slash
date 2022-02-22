package ai.dragonfly.math.interval

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

}

case class Domain[DOMAIN](interval:Interval[DOMAIN])(using `#`: Numeric[DOMAIN]) {
  val `1`:DOMAIN = `#`.one // Multiplicative Identity
  val `0`:DOMAIN = `#`.zero // Additive Identity
  def one:DOMAIN = `1`
  def zero:DOMAIN = `0`
  def min:DOMAIN = interval.min
  def MAX:DOMAIN = interval.MAX
}