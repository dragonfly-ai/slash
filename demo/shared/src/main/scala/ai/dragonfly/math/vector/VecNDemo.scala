package ai.dragonfly.math.vector

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.*
import ai.dragonfly.math.vector.Vec.*
import ai.dragonfly.math.Random
import Random.*

import narr.*

import scala.language.postfixOps

object VecNDemo extends Demonstration {

  val r = defaultRandom
  override def demo():Unit = {
    val v42a: Vec[42] = r.nextVec[42]()
    val v42b: Vec[42] = r.nextVec[42]()
    print("Random Vec[42] : ")
    println(v42a.render())
    print("In CSV format v42a.csv() : ")
    println(v42a.csv())
    print("In TSV format v42a.tsv() : ")
    println(v42a.tsv())
    print("(v42a - v42b).render() : ")
    println((v42a - v42b).render())
    println("\n")

    val v42c = Vec[42](
      0, 1, 2, 3, 4, 5, 6,
      7, 8, 9, 10, 11, 12, 13,
      14, 15, 16, 17, 18, 19, 20,
      21, 22, 23, 24, 25, 26, 27,
      28, 29, 30, 31, 32, 33, 34,
      35, 36, 37, 38, 39, 40, 41
    )
    println(v42c.render())


    // Vectors with lengths defined at runtime.
    // method 1:
    val l0:Int = r.nextInt (100)
    type N0 = l0.type
    val rtv0: Vec[N0] = r.nextVec[N0]()
    println(rtv0.render())

    // method 2:
    val l1: Int = r.nextInt(100)
    val rtv1: Vec[l1.type] = r.nextVec[l1.type]()
    println(rtv1.render())

/*
    // Unfortunately this doesn't work and throws a compiler error even though l1 == l2 is true:
    val l1: Int = r.nextInt(100)
    val l2: Int = 0 + l1
    val rtv1: Vec[l1.type] = r.nextVec[l1.type]()
    val rtv2: Vec[l2.type] = r.nextVec[l2.type]()
    println((rtv1 + rtv2).render())

    // [error] 57 |    println((rtv1 + rtv2).render())
    // [error]    |                    ^^^^
    // [error]    |             Found:    (rtv2 : ai.dragonfly.math.vector.Vec[(l2 : Int)])
    // [error]    |             Required: ai.dragonfly.math.vector.Vec[(l1 : Int)]
    // However you can do this:
    println((rtv1 + rtv2.asInstanceOf[Vec[l1.type]]).render())
*/
  }

  override def name: String = "Vec[N]"
}
