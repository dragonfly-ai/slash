package ai.dragonfly.math.stats.mesh

import ai.dragonfly.math.vector.Vector3

object TestSampleableTetrahedron extends App {

  val t = Tetrahedron(
    Vector3(1, 1, 1),
    Vector3(2, 1, 1),
    Vector3(1, 2, 1),
    Vector3(1, 1, 2)
  )

  println("# dragonfly.ai ''")
  println("o tetrahedronSamples")
  for (i <- 0 until 25) {
    val s = t.draw()
    println(s"v ${s.x} ${s.y} ${s.z}")
  }

}
