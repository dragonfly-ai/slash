package ai.dragonfly.math.vector

object VectorValues {
  def apply(values:Double*):VectorValues = {
    fill(values.size)((i:Int) => values(i))
  }

  def fill(dimension:Int)(f: Int => Double):VectorValues = {
    val values:VectorValues = new VectorValues(dimension)
    for (i <- values.indices) values(i) = f(i)
    values
  }
}