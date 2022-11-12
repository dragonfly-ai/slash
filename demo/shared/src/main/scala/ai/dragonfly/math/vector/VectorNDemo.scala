package ai.dragonfly.math.vector

import ai.dragonfly.democrossy.Demonstration

import ai.dragonfly.math.*
import Random.*
import ai.dragonfly.math.vector.Vector.*
import narr.*

/**
 * Created by clifton on 1/9/17.
 */

object VectorNDemo extends Demonstration {

  override def demo():Unit = {
    import Console.{GREEN, RED, RESET, YELLOW, UNDERLINED, RED_B}

    println("\n\nVectorN.fill(9, 0)")
    println(VectorN.fill(9, 0))

    val rvn:VectorN = defaultRandom.nextVector(42, 777.777).asInstanceOf[VectorN]
    println("\n\nval rvn:VectorN = VectorN.random(42, 777)")
      println("\n\trvn.toString: ")
      println("\n\t\t")
    println(rvn)
      println("\n\trvn.exhaustiveToString(): ")
      println("\n\t\t")
    println(rvn.exhaustiveToString())
      println("\n\trvn.exhaustiveToString(numberFormatter = (d:Double) => \"%7.3f\").format(d)): ")
      println("\n\t\t")
    println(rvn.exhaustiveToString(numberFormatter = (d:Double) => "%7.3f".format(d)))
      println("\n\trvn.indexedExhaustiveToString(): ")
      println("\n\t\t")
    println(rvn.indexedExhaustiveToString(numberFormatter = (d:Double) => "%7.3f".format(d)))
      println("\n\trvn.indexedExhaustiveToString(numberFormatter = (d:Double) => \"%7.3f\".format(d)): ")
      println("\n\t\t")
    println(rvn.indexedExhaustiveToString(numberFormatter = (d:Double) => "%7.3f".format(d)))


    println("\n\nVector.midpoint(new VectorN(1.0, 2.0, 3.0, 4.0, 5.0), VectorN(5.0, 4.0, 3.0, 2.0, 1.0))\n\t")
      println(Vector.midpoint(VectorN(1.0, 2.0, 3.0, 4.0, 5.0), VectorN(5.0, 4.0, 3.0, 2.0, 1.0)).toString)
      println("\n")

  }

  override def name: String = "VectorN"

}
