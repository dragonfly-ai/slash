package ai.dragonfly.math

import scala.quoted.Type

package object vector {
  
  type VectorIndices = native.VectorIndices
  type VectorValues = native.VectorValues
  type VECTORS = native.VECTORS
  type VectorValuesObject = native.VectorValuesObject
  val VectorValues:VectorValuesObject = native.VectorValues

  extension (v0: Vector)

    // quantifiers

    def dot(v: Vector): Double = v0 dot v0.recognize(v)
    def *(v: Vector): Double = v0 * v0.recognize(v)
    def distanceSquaredTo(v: Vector): Double = v0 distanceSquaredTo v0.recognize(v)
    def distanceTo(v: Vector): Double = v0 distanceTo v0.recognize(v)

    // in place scalar mutators
    def normalize(): Vector = v0.normalize()

    def scale(scalar: Double): Vector = v0.scale(scalar)
    def divide(divisor: Double): Vector = v0.divide(divisor)

    def add(v: Vector): Vector = v0.add(v0.recognize(v))
    def subtract(v: Vector): Vector = v0.subtract(v0.recognize(v))

    // copy operations
    def copy(): Vector = v0.copy()
    def *(d:Double): Vector = v0 * d
    def /(d:Double): Vector = v0 / d

    def +(v: Vector): Vector = v0 + v0.recognize(v)
    def -(v: Vector): Vector = v0 - v0.recognize(v)

}

