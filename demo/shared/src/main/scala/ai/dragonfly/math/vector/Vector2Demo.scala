package ai.dragonfly.math.vector

import Console.*

import narr.*

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.*
import Constant.π

import Vector.*
import Vector2.*
/**
 * Created by clifton on 1/10/17.
 */

object Vector2Demo extends Demonstration {

  override def demo():Unit = {

    val radians:NArray[Double] = NArray[Double](π/8, π/7, π/6, π/5, π/4, π/3, π/2, π)

    var di:Int = 0
    while (di < radians.length) {
      val vr = Vector[2](1, 0)
      val v = vr.copy
      val theta = radians(di)
      vr.rotate(theta)
      println(s"${v.show}.rotate($GREEN$theta$RESET) -> ${vr.show}")
      println(s"${v.show}.angleFrom(${vr.show}) -> $GREEN${v.angleFrom(vr)}$RESET\n")
      di = di + 1
    }

  }

  override def name: String = "Vector[2]"

}
