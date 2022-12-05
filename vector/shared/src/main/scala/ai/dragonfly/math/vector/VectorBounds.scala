package ai.dragonfly.math.vector

case class VectorBounds[V <: ai.dragonfly.math.vector.Vector](min: V, MAX: V) {
  def contains(v: V):Boolean = {
    var o:Boolean = true
    var i:Int = 0; while(o && i < min.dimension) {
      o = min.component(i) <= v.component(i) && v.component(i) <= MAX.component(i)
      i += 1
    }
    o
  }
}
