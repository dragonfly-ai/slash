package ai.dragonfly.math.vector

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.squareInPlace
import narr.*
import Vec.*

/**
 * Created by clifton on 1/10/17.
 */

object Vec3Demo extends Demonstration {

  override def demo():Unit = {
    val i = Vec[3](1, 0, 0)
    val j = Vec[3](0, 1, 0)
    val k = Vec[3](0, 0, 1)

    println(s"i3 X j3 -> ${(i ⨯ j).show}\n")
    println(s"j3 X i3 -> ${(j ⨯ i).show}\n")

    println(s"i3 X k3 -> ${(i ⨯ k).show}\n")
    println(s"k3 X i3 -> ${(k ⨯ i).show}\n")

    println(s"j3 X k3 -> ${(j ⨯ k).show}\n")
    println(s"k3 X j3 -> ${(k ⨯ j).show}\n")

    println(s"i3 dot j3 -> ${i dot j}\n")
    println(s"j3 dot i3 -> ${j dot i}\n")

    println(s"i3 dot k3 -> ${i dot k}\n")
    println(s"k3 dot i3 -> ${k dot i}\n")

    println(s"j3 dot k3 -> ${j dot k}\n")
    println(s"k3 dot j3 -> ${k dot j}\n")
  }

  override def name: String = "Vec[3]"
}
