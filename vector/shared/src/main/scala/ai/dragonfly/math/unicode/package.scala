package ai.dragonfly.math

package object unicode {

  private val superscriptDigits: Array[String] = Array[String]("⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹")
  private val subscriptDigits: Array[String] = Array[String]("₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", "₊")

  private def digitMapper(i: Int, digitMap: Array[String]): String = {

    var in: Int = i / 10
    var out: String = digitMap(i % 10)

    while (in > 0) {
      out = digitMap(in % 10) + out
      in = in / 10
    }
    out
  }

  def exalt(i: Int): String = {
    if (i < 0) "⁻" + digitMapper(i * -1, superscriptDigits)
    else digitMapper(i, superscriptDigits)
  }

  def abase(i: Int): String = {
    if (i < 0) "⁻" + digitMapper(i * -1, subscriptDigits)
    else digitMapper(i, subscriptDigits)
  }
}
