package ai.dragonfly.math.vector

import ai.dragonfly.math.*
import narr.*

/**
 * Created by clifton on 1/10/17.
 */

object Vector2 extends VectorCompanion[Vector2] {

  inline given dimension: Int = 2

  override inline def validDimension(dimension: Int): Boolean = dimension == this.dimension

  override def apply(values:NArray[Double]): Vector2 = new Vector2(dimensionCheck(values, dimension))

  def apply(x:Double, y:Double): Vector2 = Vector2(NArray[Double](x, y))

}

case class Vector2 private (values:NArray[Double]) extends Vector {

  type VEC = Vector2

  inline def x:Double = values(0)
  inline def y:Double = values(1)

  inline def pseudoCross(v: Vector2): Double = x * v.y + y * v.x

  inline def rotateDegrees(degrees: Double): Vector2 = rotate(degreesToRadians(degrees))

  inline def rotate(radians: Double): Vector2 = {
    val cos = Math.cos( radians )
    val sin = Math.sin( radians )

    val x1 = x*cos - y*sin
    values(1) = x*sin + y*cos
    values(0) = x1

    this
  }

  override def copy(): VEC = Vector2(x, y)

  override def toString: String = s"《²↗〉${x}ᵢ ${y}ⱼ〉" // ₂⃗ ²↗ ↗²

}
