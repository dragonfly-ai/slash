package ai.dragonfly.math.example

import ai.dragonfly.math


object TestGamma extends Demonstrable {
  override def demo(implicit sb:StringBuilder = new StringBuilder()):StringBuilder = {
    import scala.language.postfixOps

    sb.append("Demonstrate Gamma Function on Integers 1 - 10\n")
    for ( i <- 1 until 10 ) {
      val i_1:Int = i - 1
      sb.append(s"\tΓ($i):$i_1! => ${math.gamma(i.toDouble)} : ${math.Factorial(i_1)}\n")
    }
    sb
  }
  override def name:String = "Gamma"
}