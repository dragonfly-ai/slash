package ai.dragonfly.math

package object vector {
  type VectorValues = native.VectorValues
  type VECTORS = native.VECTORS

  extension (v: Vector) def scale(scalar: Double): Vector = {
    v match {
      case v2: Vector2 => v2.scale(scalar)
      case v3: Vector3 => v3.scale(scalar)
      case v4: Vector4 => v4.scale(scalar)
      case vN: VectorN => vN.scale(scalar)
    }
  }

  extension (v: Vector) def divide(divisor: Double): Vector = {
    v match {
      case v2: Vector2 => v2.divide(divisor)
      case v3: Vector3 => v3.divide(divisor)
      case v4: Vector4 => v4.divide(divisor)
      case vN: VectorN => vN.divide(divisor)
    }
  }
  extension (scalar: Double) def *(v: Vector): Vector = {
    v match {
      case v2: Vector2 => v2 * scalar
      case v3: Vector3 => v3 * scalar
      case v4: Vector4 => v4 * scalar
      case vN: VectorN => vN * scalar
    }
  }
  extension (v: Vector) def *(scalar: Double): Vector = {
    v match {
      case v2: Vector2 => v2 * scalar
      case v3: Vector3 => v3 * scalar
      case v4: Vector4 => v4 * scalar
      case vN: VectorN => vN * scalar
    }
  }
  extension (v: Vector) def /(divisor: Double): Vector = {
    v match {
      case v2: Vector2 => v2 / divisor
      case v3: Vector3 => v3 / divisor
      case v4: Vector4 => v4 / divisor
      case vN: VectorN => vN / divisor
    }
  }
  extension (v: Vector) def add(v1: Vector): Vector = {
    v match {
      case v2: Vector2 => v2.add(v1.asInstanceOf[Vector2])
      case v3: Vector3 => v3.add(v1.asInstanceOf[Vector3])
      case v4: Vector4 => v4.add(v1.asInstanceOf[Vector4])
      case vN: VectorN => vN.add(v1.asInstanceOf[VectorN])
    }
  }
  extension (v: Vector) def subtract(v1: Vector): Vector = {
    v match {
      case v2: Vector2 => v2.subtract(v1.asInstanceOf[Vector2])
      case v3: Vector3 => v3.subtract(v1.asInstanceOf[Vector3])
      case v4: Vector4 => v4.subtract(v1.asInstanceOf[Vector4])
      case vN: VectorN => vN.subtract(v1.asInstanceOf[VectorN])
    }
  }
  extension (v: Vector) def +(v1: Vector): Vector = {
    v match {
      case v2: Vector2 => v2 + v1.asInstanceOf[Vector2]
      case v3: Vector3 => v3 + v1.asInstanceOf[Vector3]
      case v4: Vector4 => v4 + v1.asInstanceOf[Vector4]
      case vN: VectorN => vN + v1.asInstanceOf[VectorN]
    }
  }
  extension (v: Vector) def -(v1: Vector): Vector = {
    v match {
      case v2: Vector2 => v2 - v1.asInstanceOf[Vector2]
      case v3: Vector3 => v3 - v1.asInstanceOf[Vector3]
      case v4: Vector4 => v4 - v1.asInstanceOf[Vector4]
      case vN: VectorN => vN - v1.asInstanceOf[VectorN]
    }
  }
  extension (v: Vector) def normalize: Vector = {
    v match {
      case v2: Vector2 => v2.normalize()
      case v3: Vector3 => v3.normalize()
      case v4: Vector4 => v4.normalize()
      case vN: VectorN => vN.normalize()
    }
  }
  extension (v: Vector) def dot(v1: Vector): Double = v * v1
  extension (v: Vector) def *(v1: Vector): Double = {
    v match {
      case v2: Vector2 => v2.dot(v1.asInstanceOf[Vector2])
      case v3: Vector3 => v3.dot(v1.asInstanceOf[Vector3])
      case v4: Vector4 => v4.dot(v1.asInstanceOf[Vector4])
      case vN: VectorN => vN.dot(v1.asInstanceOf[VectorN])
    }
  }
  extension (v: Vector) def distanceSquaredTo(v1: Vector): Double = {
    v match {
      case v2: Vector2 => v2.distanceSquaredTo(v1.asInstanceOf[Vector2])
      case v3: Vector3 => v3.distanceSquaredTo(v1.asInstanceOf[Vector3])
      case v4: Vector4 => v4.distanceSquaredTo(v1.asInstanceOf[Vector4])
      case vN: VectorN => vN.distanceSquaredTo(v1.asInstanceOf[VectorN])
    }
  }

  extension (v: Vector) def distanceTo(v1: Vector): Double = {
    v match {
      case v2: Vector2 => v2.distanceTo(v1.asInstanceOf[Vector2])
      case v3: Vector3 => v3.distanceTo(v1.asInstanceOf[Vector3])
      case v4: Vector4 => v4.distanceTo(v1.asInstanceOf[Vector4])
      case vN: VectorN => vN.distanceTo(v1.asInstanceOf[VectorN])
    }
  }

}

