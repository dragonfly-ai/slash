package ai.dragonfly.math.vector

import ai.dragonfly.math.*
import ai.dragonfly.democrossy.Demonstration

import scala.language.postfixOps
import ai.dragonfly.math.squareInPlace
import narr.*

object Vector4Demo extends Demonstration {

  override def demo():Unit = {
    val i = Vector4(1, 0, 0, 0)
    val j = Vector4(0, 1, 0, 0)
    val k = Vector4(0, 0, 1, 0)
    val l = Vector4(0, 0, 0, 1)

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

    val v0:Vector4 = Vector4(0.5, 0.0, 1.0, 0.75)
    println("val v₀ = Vector4(0.5, 0.0, 1.0, 0.75)")
    println("\n\tv₀:")
    println(v0)
    println("\nv₀.scale(3):\n\t"); println(v0.scale(3.0)); println(" /* in place operation */")
    val v1:Vector4 = Vector4(5, 6, 7, 8)
    println("\nv₁ = ")
    println(v1)
    println("\nv₁.add(v1) = "); println(v1.add(v1)); println(" /* in place operation */")
    val v2:Vector4 = Vector4(0.25, 0.25, 0.25, 0.25)
    println("\nv₂ = "); println(v2)
    println("\nv₂.dot(v₀) = "); println(v2.dot(v0))
    println("\nv₂ = "); println(v2)
    println("\nv₂.subtract(v₀) = "); println(v2.subtract(v0)); println(" /* in place operation */")

    for (i <- 0 until 10) {
      val vT:Vector4 = Vector4.random(10.0)
      println("\nval vT = Vector4.random() = "); println(vT)
      println("\n\t∥"); println(vT); println("∥ = "); println(vT.norm)
      println("\n\tvT.normalize = "); println(vT.normalize()); println(" /* in place operation */")
      println("\n\t∥"); println(vT); println("∥ = "); println(vT.norm)
      println("\n\t"); println(vT); println(" * 2.0 = "); println(vT * 2); println(" /* Copy operation */")
      println("\n\tvT remains unnaffected: "); println(vT)
    }
    println("\n")
  }

  override def name: String = "Vector4"
}
