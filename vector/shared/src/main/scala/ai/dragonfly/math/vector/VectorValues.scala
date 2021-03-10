package ai.dragonfly.math.vector

import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExportAll
object VectorValues {
  @JSExport("apply")
  def apply(values:Double*):VectorValues = {
    fill(values.size)((i:Int) => values(i))
  }

  def fill(dimension:Int)(f: Int => Double):VectorValues = {
    val values:VectorValues = new VectorValues(dimension)
    for (i <- values.indices) values(i) = f(i)
    values
  }
}