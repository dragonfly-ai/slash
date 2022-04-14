package ai.dragonfly.math

import scala.quoted.Type

package object vector {
  
  type VectorIndices = native.VectorIndices
  type VectorValues = native.VectorValues
  type VECTORS = native.VECTORS
  type VectorValuesObject = native.VectorValuesObject
  val VectorValues:VectorValuesObject = native.VectorValues

  extension (v0: Vector[_])

    // quantifiers

    def dot(v: Vector[_]): Double = v0 dot v0.recognize(v)
    def *(v: Vector[_]): Double = v0 * v0.recognize(v)
    def distanceSquaredTo(v: Vector[_]): Double = v0 distanceSquaredTo v0.recognize(v)
    def distanceTo(v: Vector[_]): Double = v0 distanceTo v0.recognize(v)

    // in place scalar mutators
    def normalize(): Vector[_] = v0.normalize()

    def scale(scalar: Double): Vector[_] = v0.scale(scalar)
    def divide(divisor: Double): Vector[_] = v0.divide(divisor)

    def add(v: Vector[_]): Vector[_] = v0.add(v0.recognize(v))
    def subtract(v: Vector[_]): Vector[_] = v0.subtract(v0.recognize(v))

    // copy operations
    def copy(): Vector[_] = v0.copy()
    def *(d:Double): Vector[_] = v0 * d
    def /(d:Double): Vector[_] = v0 / d

    def +(v: Vector[_]): Vector[_] = v0 + v0.recognize(v)
    def -(v: Vector[_]): Vector[_] = v0 - v0.recognize(v)

}

