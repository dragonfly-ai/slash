package ai.dragonfly.math.geometry

import ai.dragonfly.math.vector.Vector2

object Line {

  def trace2D(v0: Vector2, v1: Vector2, f: (Int, Int) => Unit): Unit = trace2D(
    v0.x.toInt, v0.y.toInt,
    v1.x.toInt, v1.y.toInt,
    f
  )

  // Bresenham Line Drawing Algorithm.
  def trace2D(sX: Int, sY: Int, eX: Int, eY: Int, f: (Int, Int) => Unit): Unit = {

    val steep: Boolean = Math.abs(eY - sY) > Math.abs(eX - sX)

    var x0: Int = sX
    var y0: Int = sY
    var x1: Int = eX
    var y1: Int = eY

    if (steep) {
      var temp = x0
      x0 = y0
      y0 = temp

      temp = x1
      x1 = y1
      y1 = temp
    }

    if (x0 > x1) {
      var temp = x0
      x0 = x1
      x1 = temp

      temp = y0
      y0 = y1
      y1 = temp
    }

    val dX: Int = x1 - x0
    val dY: Int = Math.abs(y1 - y0)
    var error: Float = 0
    val dError: Float = dY.toFloat / dX.toFloat
    var cY: Int = y0
    val yStep: Int = if (y0 < y1) 1 else -1

    for (cX <- x0 to x1) {

      if (steep) f(cY, cX)
      else f(cX, cY)

      error = error + dError

      if (error >= 0.5f) {
        cY = cY + yStep
        error = error - 1.0f
      }
    }
  }

}
