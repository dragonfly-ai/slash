package ai.dragonfly.math.vector

case class VectorBounds(min: Vector, MAX: Vector) {
  def contains(v:Vector):Boolean = {
    var o:Boolean = true
    var i:Int = 0
    while(o && i < min.dimension) {
      o = min.component(i) <= v.component(i) && v.component(i) <= MAX.component(i)
      i += 1
    }
    o
  }
}
