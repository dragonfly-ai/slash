package ai.dragonfly.math.vector

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.*
import ai.dragonfly.math.vector.Vector.*
import ai.dragonfly.math.Random
import Random.*

import narr.*

import scala.language.postfixOps

object VectorNDemo extends Demonstration {

  val r = defaultRandom
  override def demo():Unit = {
    val v42a: Vector[42] = r.nextVector[42]()
    val v42b: Vector[42] = r.nextVector[42]()
    print("Random Vector[42] : ")
    println(v42a.render())
    print("In CSV format v42a.csv() : ")
    println(v42a.csv())
    print("In TSV format v42a.tsv() : ")
    println(v42a.tsv())
    print("(v42a - v42b).render() : ")
    println((v42a - v42b).render())
    println("\n")
  }

  override def name: String = "Vector[N]"
}
