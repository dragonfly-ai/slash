package ai.dragonfly.math.vector

trait WeightedVector[V <: Vector] {
  var weight: Double
  def addWeight(w: Double): Unit = {
    weight = weight + w
  }
  def weighted: V
}

case class WeightedVector2(override var weight: Double, v2: Vector2) extends WeightedVector[Vector2] {
  override def weighted: Vector2 = v2.copy().scale(weight)
}

case class WeightedVector3(override var weight: Double, v3: Vector3) extends WeightedVector[Vector3] {
  override def weighted: Vector3 = v3.copy().scale(weight)
}

case class WeightedVectorN(override var weight: Double, vN: VectorN) extends WeightedVector[VectorN] {
  override def weighted: VectorN = vN.copy().scale(weight).asInstanceOf[VectorN]
}