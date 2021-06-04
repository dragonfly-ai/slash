package ai.dragonfly.math

package object vector {
  type VectorValues = native.VectorValues
  type VECTORS = native.VECTORS

  type V = Vector
  type V2 = Vector2
  type V3 = Vector3
  type V4 = Vector4
  type VN = VectorN

  val V:Vector.type = Vector
  val V2:Vector2.type  = Vector2
  val V3:Vector3.type  = Vector3
  val V4:Vector4.type  = Vector4
  val VN:VectorN.type  = VectorN
}
