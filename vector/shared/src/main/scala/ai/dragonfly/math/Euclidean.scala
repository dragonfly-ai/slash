package ai.dragonfly.math

trait Euclidean {
  val self:Euclidean = this

  def dimension: Int

  inline def inDimensionOrThrowException(i:Int):Unit = if (i < 0 || dimension < i) throw ExtraDimensionalAccessException(self, i)

  inline def sameDimensionOrThrowException(that:Euclidean):Unit = if (self.dimension != that.dimension) throw MismatchedDimensionsException(self, that)

  // read
  def component(i: Int): Double

  // write
  def component(i: Int, d:Double): Double  // returns d if successful, -1 on failure

  object euclid {

    inline def normSquared: Double = {
      var mag2 = 0.0
      for (i <- 0 until dimension) {
        mag2 = mag2 + squareInPlace(component(i))
      }
      mag2
    }

    inline def norm: Double = Math.sqrt(normSquared)

    inline def distanceSquaredTo(that: Euclidean): Double = {
      sameDimensionOrThrowException(that)

      var distance = 0.0
      for (i <- 0 until dimension) {
        distance = distance + squareInPlace(self.component(i) - that.component(i))
      }
      distance
    }

    inline def distanceTo(that: Euclidean): Double = Math.sqrt(distanceSquaredTo(that))

    inline def dot(that: Euclidean): Double = {
      sameDimensionOrThrowException(that)
      var product = 0.0
      for (i <- 0 until dimension) {
        product = product + self.component(i) * that.component(i)
      }
      product
    }


    inline def scale(scalar: Double): Unit = {
      for (i <- 0 until dimension) {
        self.component(i, self.component(i) * scalar)
      }
    }

    inline def divide(divisor: Double): Unit = {
      for (i <- 0 until dimension) {
        self.component(i, self.component(i) / divisor)
      }
    }

    inline def round(): Unit = {
      for (i <- 0 until dimension) {
        self.component(i, Math.round(self.component(i)).toDouble)
      }
    }

    inline def discretize(): Unit = round()

    inline def discretize(r: Double): Unit = {
      for (i <- 0 until dimension) {
        self.component(i, r * Math.round(self.component(i) / r).toDouble)
      }
    }

    inline def add(that: Euclidean): Unit = {
      sameDimensionOrThrowException(that)
      for (i <- 0 until dimension) {
        self.component(i, self.component(i) + that.component(i))
      }
    }

    inline def subtract(that: Euclidean): Unit = {
      sameDimensionOrThrowException(that)
      for (i <- 0 until dimension) {
        self.component(i, self.component(i) - that.component(i))
      }
    }

  }

}


case class MismatchedDimensionsException(e0:Euclidean, e1:Euclidean) extends Exception(
  s"Operation undefined on Euclidean objects with different dimensions:\n\tdim($e0) = ${e0.dimension}\n\tdim($e1) = ${e1.dimension}"
)

case class ExtraDimensionalAccessException(e:Euclidean, ci: Int) extends Exception(
  s"Index: $ci exceeds dimensionality of Euclidean object${e.dimension}: $e"
)