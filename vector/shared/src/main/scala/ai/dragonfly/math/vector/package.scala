package ai.dragonfly.math

package object vector {

  type VectorValues = native.VectorValues
  type VECTORS = native.VECTORS

  // in place mutator operations:
  extension (v0: Vector) def scale(scalar: Double): Vector = {
    v0 match {
      case v2: Vector2 => v2.scale(scalar)
      case v3: Vector3 => v3.scale(scalar)
      case v4: Vector4 => v4.scale(scalar)
      case vN: VectorN => vN.scale(scalar)
    }
  }

  extension (v0: Vector) def divide(divisor: Double): Vector = {
    v0 match {
      case v2: Vector2 => v2.divide(divisor)
      case v3: Vector3 => v3.divide(divisor)
      case v4: Vector4 => v4.divide(divisor)
      case vN: VectorN => vN.divide(divisor)
    }
  }

  extension (v0: Vector) def add(v: Vector): Vector = {
    v0 match {
      case v2: Vector2 => v2.add(v.asInstanceOf[Vector2])
      case v3: Vector3 => v3.add(v.asInstanceOf[Vector3])
      case v4: Vector4 => v4.add(v.asInstanceOf[Vector4])
      case vN: VectorN => vN.add(v.asInstanceOf[VectorN])
    }
  }

  extension (v0: Vector) def subtract(v: Vector): Vector = {
    v0 match {
      case v2: Vector2 => v2.subtract(v.asInstanceOf[Vector2])
      case v3: Vector3 => v3.subtract(v.asInstanceOf[Vector3])
      case v4: Vector4 => v4.subtract(v.asInstanceOf[Vector4])
      case vN: VectorN => vN.subtract(v.asInstanceOf[VectorN])
    }
  }

  extension (v0: Vector) def normalize: Vector = {
    v0 match {
      case v2: Vector2 => v2.normalize()
      case v3: Vector3 => v3.normalize()
      case v4: Vector4 => v4.normalize()
      case vN: VectorN => vN.normalize()
    }
  }

  // copy operations
  extension (v0: Vector) def +(v: Vector): Vector = {
    v0 match {
      case v2: Vector2 => v2 + v.asInstanceOf[Vector2]
      case v3: Vector3 => v3 + v.asInstanceOf[Vector3]
      case v4: Vector4 => v4 + v.asInstanceOf[Vector4]
      case vN: VectorN => vN + v.asInstanceOf[VectorN]
    }
  }

  extension (v0: Vector) def -(v: Vector): Vector = {
    v0 match {
      case v2: Vector2 => v2 - v.asInstanceOf[Vector2]
      case v3: Vector3 => v3 - v.asInstanceOf[Vector3]
      case v4: Vector4 => v4 - v.asInstanceOf[Vector4]
      case vN: VectorN => vN - v.asInstanceOf[VectorN]
    }
  }

  extension (scalar: Double) def *(v0: Vector): Vector = {
    v0 match {
      case v2: Vector2 => v2 * scalar
      case v3: Vector3 => v3 * scalar
      case v4: Vector4 => v4 * scalar
      case vN: VectorN => vN * scalar
    }
  }

  extension (v0: Vector) def *(scalar: Double): Vector = {
    v0 match {
      case v2: Vector2 => v2 * scalar
      case v3: Vector3 => v3 * scalar
      case v4: Vector4 => v4 * scalar
      case vN: VectorN => vN * scalar
    }
  }

  extension (v0: Vector) def /(divisor: Double): Vector = {
    v0 match {
      case v2: Vector2 => v2 / divisor
      case v3: Vector3 => v3 / divisor
      case v4: Vector4 => v4 / divisor
      case vN: VectorN => vN / divisor
    }
  }


  // other operators
  extension (v0: Vector) def *(v: Vector): Double = v dot v

  extension (v0: Vector) def dot(v: Vector): Double = {
    v0 match {
      case v2: Vector2 => v2.dot(v.asInstanceOf[Vector2])
      case v3: Vector3 => v3.dot(v.asInstanceOf[Vector3])
      case v4: Vector4 => v4.dot(v.asInstanceOf[Vector4])
      case vN: VectorN => vN.dot(v.asInstanceOf[VectorN])
    }
  }

  extension (v0: Vector) def distanceSquaredTo(v: Vector): Double = {
    v0 match {
      case v2: Vector2 => v2.distanceSquaredTo(v.asInstanceOf[Vector2])
      case v3: Vector3 => v3.distanceSquaredTo(v.asInstanceOf[Vector3])
      case v4: Vector4 => v4.distanceSquaredTo(v.asInstanceOf[Vector4])
      case vN: VectorN => vN.distanceSquaredTo(v.asInstanceOf[VectorN])
    }
  }

  extension (v0: Vector) def distanceTo(v: Vector): Double = Math.sqrt(v0.distanceSquaredTo(v))

}

