package ai.dragonfly.math.vector

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.*
import ai.dragonfly.math.Random.*
import ai.dragonfly.math.vector.Vec.*
import ai.dragonfly.math.vector.Vec2.*
import narr.*

import scala.language.postfixOps

object MixedVecDemo extends Demonstration {

  val r = defaultRandom

  override def demo():Unit = {
    val v2:Vec[2] = r.nextVec[2]()
    val v3:Vec[3] = r.nextVec[3]()
    val v4:Vec[4] = r.nextVec[4]()

    println(s"${v2.show}.x : ${v2.x}")
    println(s"${v3.show}.x : ${v3.x}")
    println(s"${v4.show}.x : ${v4.x}")

    println(s"${v2.show}.y : ${v2.y}")
    println(s"${v3.show}.y : ${v3.y}")
    println(s"${v4.show}.y : ${v4.y}")

    println(s"${v2.show}.z : Compiler Error!")
    println(s"${v3.show}.z : ${v3.z}")
    println(s"${v4.show}.z : ${v4.z}")

    println(s"${v2.show}.w : Compiler Error!")
    println(s"${v3.show}.w : Compiler Error!")
    println(s"${v4.show}.w : ${v4.w}")

    val t: (
      Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double
    ) = (1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
    println(s"${Vec.fromTuple(t).show}")

  }

  override def name: String = "MixedVecDemo"
}
