/*
 * Copyright 2023 dragonfly.ai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ai.dragonfly.math.spatial

import narr.*
import ai.dragonfly.math.vector.*
import Vec3.*

import scala.collection.{Iterator, mutable}

/*
This implementation of octree lacks:
  - removal method
  - spatially ordered iterator
 */

trait PointRegionOctreeNode[+T] {
  def center: Vec[3]
  def width: Double
  var size: Int = 0

  private val nCorner = Vec[3]( center.x - width, center.y - width, center.z - width )
  private val pCorner = Vec[3]( center.x + width, center.y + width, center.z + width )

  def intersects(p: Vec[3], radiusSquared: Double):Boolean = {
    var distSquared = radiusSquared
    if (p.x < nCorner.x) {
      val diff = p.x - nCorner.x
      distSquared -= diff * diff
    } else if (p.x > pCorner.x) {
      val diff = p.x - pCorner.x
      distSquared -= diff * diff
    }
    if (p.y < nCorner.y) {
      val diff = p.y - nCorner.y
      distSquared -= diff * diff
    } else if (p.y > pCorner.y) {
      val diff = p.y - pCorner.y
      distSquared -= diff * diff
    }
    if (p.z < nCorner.z) {
      val diff = p.z - nCorner.z
      distSquared -= diff * diff
    } else if (p.z > pCorner.z) {
      val diff = p.z - pCorner.z
      distSquared -= diff * diff
    }

    distSquared > 0
  }

  def nearestNeighbor(queryPoint: Vec[3]): Option[Vec[3]]

  def insert(p: Vec[3]): PointRegionOctreeNode[T]

  def radialQuery(p: Vec[3], radiusSquared: Double): List[Vec[3]]

}

// Weights?
// removal?

class PointRegionOctree[T](width: Double, center:Vec[3] = Vec[3](0.0, 0.0, 0.0), nodeCapacity:Int = 32, minDepth:Int = 0, depthMAX:Int =  10) extends Iterable[(Vec[3], T)] {

  if (depthMAX < minDepth) throw new Exception(s"maxDepth: $depthMAX must exceed minDepth : $minDepth")

  val map: mutable.HashMap[Vec[3], T] = mutable.HashMap[Vec[3], T]()

  private var root: PointRegionOctreeNode[T] = if (minDepth == 1) {
    new PROctreeMapLeafNode[T](width, center, nodeCapacity, minDepth, depthMAX)
  } else {
    new PROctreeMapMetaNode[T](width, center, nodeCapacity, minDepth, depthMAX)
  }

  def insert(p: Vec[3], value: T): Unit = synchronized {
    root = root.insert(p)
    map.put(p, value)
  }

  def nearestNeighbor(queryPoint: Vec[3]): Option[(Vec[3], T)] = for {
    np <- root.nearestNeighbor(queryPoint)
    value <- map.get(np)
  } yield {
    (np, value)
  }

  def radialQuery(p: Vec[3], radius: Double): List[(Vec[3], T)] = {
    var matches = List[(Vec[3], T)]()

    for (k <- root.radialQuery(p, radius * radius)) {
      map.get(k) match {
        case Some(v: T) => matches = (k, v) :: matches
        case None =>
      }
      //println(matches)
    }
    //println(s"Returing from radialQuery($p, $radius)")
    matches
  }

  override def size: Int = root.size

  override def iterator: Iterator[(Vec[3], T)] = map.iterator
}

object Octant {

  def apply[T](
    factory:(Double,Vec[3],Int,Int,Int) => PointRegionOctreeNode[T],
    width: Double, center:Vec[3], nodeCapacity:Int, minDepth:Int, depthMAX:Int
  ): NArray[NArray[NArray[PointRegionOctreeNode[T]]]] = {

    val childWidth:Double = width / 2.0

    val mnD:Int = Math.max(0, minDepth - 1)
    val dMX:Int = Math.max(0, depthMAX - 1)

    NArray[NArray[NArray[PointRegionOctreeNode[T]]]] (
      NArray[NArray[PointRegionOctreeNode[T]]]( // - X
        NArray[PointRegionOctreeNode[T]](      // - Y
          factory(childWidth, Vec[3](center.x - childWidth, center.y - childWidth, center.z - childWidth), nodeCapacity, mnD, dMX), // -X-Y-Z -> 0
          factory(childWidth, Vec[3](center.x - childWidth, center.y - childWidth, center.z + childWidth), nodeCapacity, mnD, dMX)  // -X-Y+Z -> 1
        ),
        NArray[PointRegionOctreeNode[T]](      // + Y
          factory(childWidth, Vec[3](center.x - childWidth, center.y + childWidth, center.z - childWidth), nodeCapacity, mnD, dMX), // -X+Y-Z -> 0
          factory(childWidth, Vec[3](center.x - childWidth, center.y + childWidth, center.z + childWidth), nodeCapacity, mnD, dMX)  // -X+Y+Z -> 1
        )
      ),
      NArray[NArray[PointRegionOctreeNode[T]]]( // + X
        NArray[PointRegionOctreeNode[T]](      // - Y
          factory(childWidth, Vec[3](center.x + childWidth, center.y - childWidth, center.z - childWidth), nodeCapacity, mnD, dMX), // +X-Y-Z -> 0
          factory(childWidth, Vec[3](center.x + childWidth, center.y - childWidth, center.z + childWidth), nodeCapacity, mnD, dMX)  // +X-Y+Z -> 1
        ),
        NArray[PointRegionOctreeNode[T]](      // + Y
          factory(childWidth, Vec[3](center.x + childWidth, center.y + childWidth, center.z - childWidth), nodeCapacity, mnD, dMX), // +X+Y-Z -> 0
          factory(childWidth, Vec[3](center.x + childWidth, center.y + childWidth, center.z + childWidth), nodeCapacity, mnD, dMX)  // +X+Y+Z -> 1
        )
      )
    )
  }
}

class PROctreeMapMetaNode[T](override val width: Double, override val center:Vec[3], nodeCapacity:Int, minDepth:Int, depthMAX:Int) extends PointRegionOctreeNode[T] {

  // Represent Octants in an array
  val nodes: NArray[NArray[NArray[PointRegionOctreeNode[T]]]] = Octant(
    if (depthMAX <= 1) {
      (w: Double, c:Vec[3], _:Int, _:Int, _:Int) => new PROctreeMapMaxDepthNode[T](w, c)
    } else if (minDepth > 1) {
      (w: Double, c:Vec[3], nc:Int, mnD:Int, dMX:Int) => new PROctreeMapMetaNode[T](w, c, nc, mnD, dMX)
    } else {
      (w: Double, c:Vec[3], nc:Int, mnD:Int, dMX:Int) => new PROctreeMapLeafNode[T](w, c, nc, mnD, dMX)
    },
    width,
    center,
    nodeCapacity,
    minDepth,
    depthMAX
  )

  override def insert(p: Vec[3]): PointRegionOctreeNode[T] = {
    val x = if (p.x - center.x < 0.0) 0 else 1
    val y = if (p.y - center.y < 0.0) 0 else 1
    val z = if (p.z - center.z < 0.0) 0 else 1

    val insertedNode = nodes(x)(y)(z).insert(p)
    nodes(x)(y)(z) = insertedNode
    size = size + 1
    insertedNode
  }

  override def nearestNeighbor(queryPoint: Vec[3]): Option[Vec[3]] = {
    val x = if (queryPoint.x - center.x < 0.0) 0 else 1
    val y = if (queryPoint.y - center.y < 0.0) 0 else 1
    val z = if (queryPoint.z - center.z < 0.0) 0 else 1

    var node = nodes(x)(y)(z)
    if (node.size > 0) {
      node.nearestNeighbor(queryPoint)
    } else {
      for (xi <- 0 to 1; yi <- 0 to 1; zi <- 0 to 1) {
        node = nodes(xi)(yi)(zi)
        if (node.size > 0) return node.nearestNeighbor(queryPoint)
      }
      None
    }

  }

  override def radialQuery(p: Vec[3], radiusSquared: Double): List[Vec[3]] = {

    var matches = List[Vec[3]]()

    for ( x <- 0 to 1; y <- 0 to 1; z <- 0 to 1 ) {
      if (nodes(x)(y)(z).intersects(p, radiusSquared)) {
        matches = matches :++ nodes(x)(y)(z).radialQuery(p, radiusSquared)
      }
      println(matches)
    }

    matches
  }

}

class PROctreeMapLeafNode[+T](override val width: Double, override val center:Vec[3], nodeCapacity:Int, minDepth:Int, depthMAX:Int) extends PointRegionOctreeNode[T] {
  val points: NArray[Vec[3]] = NArray.fill[Vec[3]](nodeCapacity)(null.asInstanceOf[Vec[3]])

  def insert(p: Vec[3]): PointRegionOctreeNode[T] = {
    if (size < points.length ) {
      points(size) = p
      size = size + 1
      this
    } else split().insert(p)
  }

  def split(): PointRegionOctreeNode[T] = {

    val replacementNode = {
      if (depthMAX == 0) new PROctreeMapMaxDepthNode[T](width, center)
      else new PROctreeMapMetaNode[T](width, center, nodeCapacity, minDepth - 1, depthMAX - 1)
    }

    var p:Int = 0;
    while (p < points.length) {
      replacementNode.insert(points(p))
      p += 1
    }

    replacementNode
  }

  override def radialQuery(p: Vec[3], radiusSquared: Double): List[Vec[3]] = {
    var matches = List[Vec[3]]()

    var pi: Int = 0;
    while (pi < points.length) {
      val pC = points(pi)
      if (pC.euclideanDistanceSquaredTo(p) <= radiusSquared) matches = pC :: matches
      pi += 1
    }

    matches
  }

  override def nearestNeighbor(queryPoint: Vec[3]): Option[Vec[3]] = {
    var minDistSquared = Double.MaxValue
    var closestPoint: Option[Vec[3]] = None

    var pi: Int = 0;
    while (pi < points.length) {
      val pC = points(pi)
      val dist = pC.euclideanDistanceSquaredTo(queryPoint)
      if (dist < minDistSquared) {
        minDistSquared = dist
        closestPoint = Some(pC)
      }
      pi += 1
    }

    closestPoint
  }

}

class PROctreeMapMaxDepthNode[+T](override val width: Double, override val center:Vec[3] = Vec[3](0.0, 0.0, 0.0)) extends PointRegionOctreeNode[T] {
  var points: List[Vec[3]] = List[Vec[3]]()

  def insert(p: Vec[3]): PointRegionOctreeNode[T] = {
    points = p :: points
    size = size + 1
    this
  }

  override def radialQuery(p: Vec[3], radiusSquared: Double): List[Vec[3]] = {
    var matches = List[Vec[3]]()

    for (pC <- points) {
      if (pC.euclideanDistanceSquaredTo(p) <= radiusSquared) matches = pC :: matches
    }

    matches
  }

  override def nearestNeighbor(queryPoint: Vec[3]): Option[Vec[3]] = {
    var minDistSquared = Double.MaxValue
    var closestPoint: Option[Vec[3]] = None

    for (pC <- points) {
      val dist = pC.euclideanDistanceSquaredTo(queryPoint)
      if (dist < minDistSquared) {
        minDistSquared = dist
        closestPoint = Some(pC)
      }
    }

    closestPoint
  }

}