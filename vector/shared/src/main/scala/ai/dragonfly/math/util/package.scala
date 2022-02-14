package ai.dragonfly.math

import ai.dragonfly.math.util.{Demonstrable, Factorial, logGamma}
import ai.dragonfly.math.vector.VectorValues

package object util {

  // ported from https://commons.apache.org/proper/commons-math/javadocs/api-3.6.1/src-html/org/apache/commons/math3/special/Gamma.html

  private val lanczos_coefficients: VectorValues = VectorValues(
    0.99999999999999709182,
    57.156235665862923517,
    -59.597960355475491248,
    14.136097974741747174,
    -0.49191381609762019978,
    0.33994649984811888699e-4,
    0.46523628927048575665e-4,
    -0.98374475304879564677e-4,
    0.15808870322491248884e-3,
    -0.21026444172410488319e-3,
    0.21743961811521264320e-3,
    -0.16431810653676389022e-3,
    0.84418223983852743293e-4,
    -0.26190838401581408670e-4,
    0.36899182659531622704e-5
  )

  private inline def lanczos(x: Double):Double = {
    var sum = lanczos_coefficients(0)
    for (i <- 1 until lanczos_coefficients.length) {
      sum = sum + ( lanczos_coefficients(i) / (x+i))
    }
    sum
  }

  def Γ(x:Double):Double = Math.exp(logGamma(x))
  def gamma(x:Double):Double = Γ(x)

  def B(α:Double, β:Double):Double = ( Γ(α) * Γ(β) ) / Γ(α + β)
  def beta(α:Double, β:Double):Double = B(α, β)

  private inline val c1 = 0.5 + (607.0 / 128.0)
  private inline val c2 = 0.9189385332046727 //(Math.log(2.0 * Math.PI) / 2.0).toDouble
  //println(s"c2 = $c2")

  // ported from apache commons math
  def logGamma(x:Double):Double = if (x == Double.NaN || (x <= 0.0)) {
    throw InvalidArgumentToGammaFunction(x, s"logGamma( x = $x ) is only defined on x > 0. ")
  } else if (x < 0.5) {
    logGamma1p(x) - Math.log(x)
  } else if (x <= 2.5) {
    logGamma1p((x - 0.5) - 0.5)
  } else if (x <= 8.0) {
    val n:Int = Math.floor(x - 1.5).toInt
    var prod = 1.0
    for (i <- 1 to n) {
      prod = prod * ( x - i )
    }
    logGamma1p(x - (n + 1)) + Math.log(prod)
  } else {
    val sum = lanczos(x)
    val tmp = x + c1
    ((x + 0.5) * Math.log(tmp)) - tmp + c2 + Math.log(sum / x)
  }


  private def logGamma1p(x:Double):Double = {
    if (x < -0.5 || x > 1.5) throw new InvalidArgumentToGammaFunction(x, s"logGamma1p( x = $x ) defined on -0.5 > x > 1.5")
    -Math.log1p(invGamma1pm1(x))
  }

  // Constants copied from org/apache/commons/math3/special/Gamma.html copied from DGAM1 in the NSWC library.

  val A0 = 0.611609510448141581788E-08
  val A1 = 0.624730830116465516210E-08
  val B1 = 0.203610414066806987300E+00
  val B2 = 0.266205348428949217746E-01
  val B3 = 0.493944979382446875238E-03
  val B4 = -0.851419432440314906588E-05
  val B5 = -0.643045481779353022248E-05
  val B6 = 0.992641840672773722196E-06
  val B7 = -0.607761895722825260739E-07
  val B8 = 0.195755836614639731882E-09
  val P0 = 0.6116095104481415817861E-08
  val P1 = 0.6871674113067198736152E-08
  val P2 = 0.6820161668496170657918E-09
  val P3 = 0.4686843322948848031080E-10
  val P4 = 0.1572833027710446286995E-11
  val P5 = -0.1249441572276366213222E-12
  val P6 = 0.4343529937408594255178E-14
  val Q1 = 0.3056961078365221025009E+00
  val Q2 = 0.5464213086042296536016E-01
  val Q3 = 0.4956830093825887312020E-02
  val Q4 = 0.2692369466186361192876E-03
  val C = -0.422784335098467139393487909917598E+00
  val C0 = 0.577215664901532860606512090082402E+00
  val C1 = -0.655878071520253881077019515145390E+00
  val C2 = -0.420026350340952355290039348754298E-01
  val C3 = 0.166538611382291489501700795102105E+00
  val C4 = -0.421977345555443367482083012891874E-01
  val C5 = -0.962197152787697356211492167234820E-02
  val C6 = 0.721894324666309954239501034044657E-02
  val C7 = -0.116516759185906511211397108401839E-02
  val C8 = -0.215241674114950972815729963053648E-03
  val C9 = 0.128050282388116186153198626328164E-03
  val C10 = -0.201348547807882386556893914210218E-04
  val C11 = -0.125049348214267065734535947383309E-05
  val C12 = 0.113302723198169588237412962033074E-05
  val C13 = -0.205633841697760710345015413002057E-06

  private def invGamma1pm1(x: Double)  = if (x < -0.5 || x > 1.5) {
    throw new InvalidArgumentToGammaFunction(x, s"invGamma1pm1( x = $x ) defined on -0.5 > x > 1.5")
  } else {
    val t = if (x <= 0.5) x else (x - 0.5) - 0.5
    if (t < 0.0) {
      val a = A0 + t * A1
      var b = B8
      b = B7 + t * b
      b = B6 + t * b
      b = B5 + t * b
      b = B4 + t * b
      b = B3 + t * b
      b = B2 + t * b
      b = B1 + t * b
      b = 1.0 + t * b
      var c = C13 + t * (a / b)
      c = C12 + t * c
      c = C11 + t * c
      c = C10 + t * c
      c = C9 + t * c
      c = C8 + t * c
      c = C7 + t * c
      c = C6 + t * c
      c = C5 + t * c
      c = C4 + t * c
      c = C3 + t * c
      c = C2 + t * c
      c = C1 + t * c
      c = C + t * c
      if (x > 0.5) t * c / x
      else x * ((c + 0.5) + 0.5)
    } else {
      var p = P6
      p = P5 + t * p
      p = P4 + t * p
      p = P3 + t * p
      p = P2 + t * p
      p = P1 + t * p
      p = P0 + t * p
      var q = Q4
      q = Q3 + t * q
      q = Q2 + t * q
      q = Q1 + t * q
      q = 1.0 + t * q
      var c = C13 + (p / q) * t
      c = C12 + t * c
      c = C11 + t * c
      c = C10 + t * c
      c = C9 + t * c
      c = C8 + t * c
      c = C7 + t * c
      c = C6 + t * c
      c = C5 + t * c
      c = C4 + t * c
      c = C3 + t * c
      c = C2 + t * c
      c = C1 + t * c
      c = C0 + t * c
      if (x > 0.5) (t / x) * ((c - 0.5) - 0.5)
      else x * c
    }
  }

}

object TestGamma extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    import scala.language.postfixOps

    sb.append("Demonstrate Gamma Function on Integers 1 - 10\n")
    for ( i <- 1 until 10 ) {
      val i_1:Int = i - 1
      sb.append(s"\tΓ($i):$i_1! => ${util.gamma(i.toDouble)} : ${Factorial(i_1)}\n")
    }
    sb
  }
  override def name:String = "Gamma"
}

case class InvalidArgumentToGammaFunction(x:Double, s:String) extends Exception(s) {

}