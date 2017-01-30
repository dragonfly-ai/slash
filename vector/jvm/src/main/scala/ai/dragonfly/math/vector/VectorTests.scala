package ai.dragonfly.math.vector

/**
 * Created by clifton on 1/9/17.
 */

object TestVectorN extends App {
  val v0 = new VectorN(Array[Double](0.5, 0.0, 1.0, 0.75))
  println("v0 = " + v0)
  println("v0.scale(3) = " + v0.scale(3))
  val v1 = new VectorN(Array[Double](5, 6, 7, 8))
  println("v1 = " + v1)
  println("v1.add(v1) = " + v1.add(v1))
  val v2 = new VectorN(Array[Double](0.25, 0.25, 0.25, 0.25))
  println("v2 = " + v2)
  println("v2.dot(v0) = " + v2.dot(v0))
  println("v2 = " + v2)
  println("v2.subtract(v0) = " + v2.subtract(v0))
  for (i <- 0 until 10) {
    val vT = VectorN.random(4)
    println("VectorMath.random(4) => " + vT + " magnitude = " + vT.magnitude() +
      "\n\tnormalized to: " + vT.normalize() +
      "\n\tmagnitude: " + vT.magnitude() +
      "\n\tclone: " + vT.clone() + " clone.scale(2): " + vT.clone().scale(2)
    )
    println(vT)
  }
  try {
    VectorN.random(2).subtract(VectorN.random(3))
  } catch {
    case e: Throwable => println(e)
  }

  println(VectorN.fill(9, 0))
  println(VectorN.fill(9, 0).normalize())
  println("VectorMath.random(40, Integer.MAX_VALUE) => " + VectorN.random(40, Integer.MAX_VALUE))

  println("midpoint: " + VectorN.midpoint(new VectorN(1.0, 2.0, 3.0, 4.0, 5.0), new VectorN(5.0, 4.0, 3.0, 2.0, 1.0)))
}

object TestVector2 extends App {

  for (deg <- Array[Double](10, 25, 33.333333, 45, 60, 75, 90)) {
    val i = new Vector2(1, 0)
    i.rotateDegrees(deg)
    println(s"${new Vector2(1, 0)}.rotateDegrees($deg) -> " + i)
  }

}

object TestVector3 extends App {
  val i = new Vector3(1, 0, 0)
  val j = new Vector3(0, 1, 0)
  val k = new Vector3(0, 0, 1)

  println("i X j -> " + (i X j))
  println("j X i -> " + (j X i))

  println("i X k -> " + (i X k))
  println("k X i -> " + (k X i))

  println("j X k -> " + (j X k))
  println("k X j -> " + (k X j))


  println("i dot j -> " + (i dot j))
  println("j dot i -> " + (j dot i))

  println("i dot k -> " + (i dot k))
  println("k dot i -> " + (k dot i))

  println("j dot k -> " + (j dot k))
  println("k dot j -> " + (k dot j))
}