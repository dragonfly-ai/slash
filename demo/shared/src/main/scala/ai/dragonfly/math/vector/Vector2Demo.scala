package ai.dragonfly.math.vector

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.*
import narr.*

/**
 * Created by clifton on 1/10/17.
 */

object Vector2Demo extends Demonstration {

  override def demo():Unit = {
    val degrees:Array[Double] = Array[Double](10, 25, 33.333333, 45, 60, 75, 90)
    var di:Int = 0
    while (di < degrees.length) {
      val i = Vector2(1, 0)
      val theta = degrees(di)
      i.rotateDegrees(theta)
      println(s"${Vector2(1, 0)}.rotateDegrees($theta) -> $i\n")
      di = di + 1
    }
    println(s"Vector2(0.115, 0.937).euclid.equals(Vector2(0.115, 0.937)) => true : ${Vector2(0.115, 0.937).euclid.equals(Vector2(0.115, 0.937))}\n")
    println(s"Vector2(0.115, 0.937) == Vector2(0.115, 0.937) => false : ${ Vector2(0.115, 0.937) == Vector2(0.115, 0.937) }\n")
  }

  override def name: String = "Vector2"

}
