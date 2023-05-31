package ai.dragonfly.math.vector

import ai.dragonfly.math.*
import ai.dragonfly.democrossy.Demonstration

import scala.language.postfixOps
import ai.dragonfly.math.squareInPlace
import Vec.*
import narr.*

object Vec4Demo extends Demonstration {

  override def demo():Unit = {
    val i = Vec[4](1, 0, 0, 0)
    val j = Vec[4](0, 1, 0, 0)
    val k = Vec[4](0, 0, 1, 0)
    val l = Vec[4](0, 0, 0, 1)

    println(s"i4 dot j4 -> ${i dot j}\n")
    println(s"j4 dot i4 -> ${j dot i}\n")

    println(s"i4 dot k4 -> ${i dot k}\n")
    println(s"k4 dot i4 -> ${k dot i}\n")

    println(s"j4 dot k4 -> ${j dot k}\n")
    println(s"k4 dot j4 -> ${k dot j}\n")

    println(s"i4 dot l4 -> ${i dot l}\n")
    println(s"l4 dot i4 -> ${l dot i}\n")

    println(s"j4 dot l4 -> ${j dot l}\n")
    println(s"l4 dot j4 -> ${l dot j}\n")

    println(s"k4 dot l4 -> ${k dot l}\n")
    println(s"l4 dot k4 -> ${l dot k}\n")

    println(s"Unary Minus: -i = ${ (-i).show }\n")

    val v0:Vec[4] = Vec[4](0.5, 0.0, 1.0, 0.75)
    println("val v₀ = Vec[4](0.5, 0.0, 1.0, 0.75)")
    print("\n\tv₀:"); println(v0.show)
    print("\nv₀.scale(3):\n\t"); print(v0.scale(3.0).show); println(" /* in place operation */")
    val v1:Vec[4] = Vec[4](5, 6, 7, 8)
    println("\nv₁ = "); println(v1.show)
    print("\nv₁.add(v1) = "); print(v1.add(v1).show); println(" /* in place operation */")
    val v2:Vec[4] = Vec[4](0.25, 0.25, 0.25, 0.25)
    print("\nv₂ = "); println(v2.show)
    print("\nv₂.dot(v₀) = "); println(v2.dot(v0))
    print("\nv₂ = "); println(v2.show)
    print("\nv₂.subtract(v₀) = "); print(v2.subtract(v0).show); println(" /* in place operation */")

    for (i <- 0 until 10) {
      val vT:Vec[4] = Vec.random[4](10.0)
      print("\nval vT = Vec[4].random() = "); print(vT.show)
      print("\n\t∥"); print(vT.show); print("∥ = "); println(vT.norm)
      print("\n\tvT.normalize = "); println(vT.normalize().show); println(" /* in place operation */")
      print("\n\t∥"); print(vT.show); print("∥ = "); println(vT.norm)
      print("\n\t"); print(vT.show); print(" * 2.0 = "); println((vT * 2).show); println(" /* Copy operation */")
      print("\n\tvT remains unnaffected: "); println(vT.show)
    }
    println("\n")
  }

  override def name: String = "Vec[4]"
}
