package ai.dragonfly.math

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math
import ai.dragonfly.math.Constant.π

object LogDemo extends Demonstration {

  override def demo():Unit = {
    import scala.language.postfixOps

    println("Demonstrate Log[BASE](x:Double)\n")

    println( s"log[2.0](42.0) = ${log[2.0](42.0)}" )
    var i: Int = 1; while (i > 0) {
      println( s"log[2]($i) = ${log[2](i)}" )
      i = i << 1
    }

    println(s"log[3.141592653589793](21) = ${log[3.141592653589793](21)}")

    println("Demonstrate Log(base:Double)\n")

    val logBasePi: Log = Log(π)
    i = 0; while (i < 10) {
      val x: Double = Math.random() * i
      println(s"logBasePi($x) = ${logBasePi(x)}")
      i += 1
    }


  }

  override def name:String = "Gamma"

}
