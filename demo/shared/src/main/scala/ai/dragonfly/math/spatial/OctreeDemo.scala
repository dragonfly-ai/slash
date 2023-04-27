package ai.dragonfly.math.spatial

import ai.dragonfly.democrossy.Demonstration
import ai.dragonfly.math.vector.*
import Vec3.*
import ai.dragonfly.math.spatial.*
import ai.dragonfly.math.Random.{defaultRandom as r, *}

object OctreeDemo extends Demonstration {

  override def demo():Unit = {

    // Test node intersection
    val metaNode = new PROctreeMapMetaNode[Int](100.0, Vec[3](50.0, 50.0, 50.0), 10, 0, 2)

    println("\nTest node : Vector with Radius intersections:")
    println(s"metaNode.intersects(Vec[3](1.0, 1.0, 4.0), 10), 10) ? ${metaNode.intersects(Vec[3](1.0, 1.0, 4.0), 10)}")
    println(s"metaNode.intersects(Vec[3](100.0, 100.0, 100.0), 9.9999) ? ${metaNode.intersects(Vec[3](100.0, 100.0, 100.0), 9.9999)}")
    println(s"metaNode.intersects(Vec[3](100.0, 100.0, 100.0), 10) ? ${metaNode.intersects(Vec[3](100.0, 100.0, 100.0), 10)}")
    println(s"metaNode.intersects(Vec[3](110.0, 110.0, 110.0), 10.1) ? ${metaNode.intersects(Vec[3](110.0, 110.0, 110.0), 10.1)}")
    println(s"metaNode.intersects(Vec[3](110.0, 110.0, 110.0), 11) ? ${metaNode.intersects(Vec[3](110.0, 110.0, 110.0), 11)}")

    // Test Octree
    val ot = new PointRegionOctree[Int](100.0, Vec[3](50.0, 50.0, 50.0))
    //for (i <- 0 until 5000000) {
    for (i <- 0 until 10000) {
      ot.insert( r.nextVec[3](100.0), r.nextInt() )
    }

    val queryVector = r.nextVec[3](100.0)

    val radius = 5
    val radiusSquared = radius * radius

    println("\nTest Radial Querying:")
    println(s"Query Vector: ${queryVector.show}  Radius: $radius  RadiusSquared: $radiusSquared")

    for ((v, i) <- ot.radialQuery(queryVector, radius)) {
      println(s"found vector: ${v.show} mapped to: $i, ${queryVector.euclideanDistanceTo(v)} units away from the query vector")
    }

    println("\nTest Nearest Neighbor")
    for (i <- 0 until 10) {
      val qv: Vec[3] = r.nextVec[3](100.0)

      ot.nearestNeighbor(qv) match {
        case Some((nn, i)) => println(s"${qv.show}'s nearest neighbor is: ${nn.show}:$i at distance: ${qv.euclideanDistanceTo(nn)}")
        case None => println(s"${qv.show} has no nearest neighbor?  If $ot.size != 0, you found a bug!")
      }

    }

    //  println("Test Iterator:")
    //  for ((v: Vec[3], i: Int) <- ot.iterator) {
    //    println(s"$v -> $i")
    //  }
  }

  override def name: String = "Octree"
}
