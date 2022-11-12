package ai.dragonfly.math

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.*
import ai.dragonfly.math.Factorial.!
import scala.language.postfixOps

object FactorialDemo extends Demonstration {

  override def demo():Unit = {
    for (x:Int <- Seq(1, 2, 3, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096)) {
      println(s"\t${x}! = ${x!}\n")
    }
  }

  def name:String = "Factorial"
}
