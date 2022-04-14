package ai.dragonfly.math.visualization

trait Euclidean[T] {
  def euclideanNormSquared:Double
  def euclideanNorm:Double = Math.sqrt(euclideanNormSquared)
  def euclideanDistanceSquaredTo(that:T):Double
  def euclideanDistanceTo(that:T):Double = Math.sqrt(euclideanDistanceSquaredTo(that))
}
