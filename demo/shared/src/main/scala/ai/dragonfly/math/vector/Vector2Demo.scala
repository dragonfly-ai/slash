package ai.dragonfly.math.vector

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.*
import narr.*

import Vector.*
import Vector2.*
/**
 * Created by clifton on 1/10/17.
 */

object Vector2Demo extends Demonstration {

  override def demo():Unit = {
    val degrees:NArray[Double] = NArray[Double](10, 25, 33.333333, 45, 60, 75, 90)
    var di:Int = 0
    while (di < degrees.length) {
      val i = Vector[2](1, 0)
      val theta = degrees(di)
      i.rotateDegrees(theta)
      println(s"${Vector[2](1, 0).show}.rotateDegrees($theta) -> ${i.show}\n")
      di = di + 1
    }

  }

  override def name: String = "Vector[2]"

}
