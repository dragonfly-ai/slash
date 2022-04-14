package ai.dragonfly.math.vector

package object native {

  type VectorIndices = Array[Int]
  type VectorValues = Array[Double]
  type VECTORS = Array[VectorData]
  type VectorValuesObject = Array.type
  val VectorValues:VectorValuesObject = Array
}
