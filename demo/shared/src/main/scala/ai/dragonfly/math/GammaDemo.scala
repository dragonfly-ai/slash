package ai.dragonfly.math

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math

object GammaDemo extends Demonstration {

  override def demo():Unit = {
    import scala.language.postfixOps

    println("Demonstrate Gamma Function on Integers 1 - 10\n")
    for ( i <- 1 until 10 ) {
      val i_1:Int = i - 1
      println(s"\tΓ($i):$i_1! => ${math.gamma(i.toDouble)} : ${math.Factorial(i_1)}\n")
    }

  }

  override def name:String = "Gamma"

}
