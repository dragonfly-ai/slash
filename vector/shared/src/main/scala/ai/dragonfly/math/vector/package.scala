package ai.dragonfly.math

import scala.quoted.Type

package object vector {

  type VectorValues = native.VectorValues
  type VECTORS = native.VECTORS
  type VectorValuesObject = native.VectorValuesObject
  val VectorValues:VectorValuesObject = native.VectorValues

  def leafVector[V <: Vector](x: V): VectorOps[V] = x match {
    case x: VectorOps[V] => x
  }

  extension (v0: Vector)

    // quantifiers
    def magnitudeSquared: Double = euclideanNormSquared
    def magnitude: Double = euclideanNorm

    def euclideanNormSquared: Double = { leafVector(v0).euclideanNormSquared }
    def euclideanNorm: Double = { leafVector(v0).euclideanNorm }

    def dot(v: Vector): Double = { val lv = leafVector(v0); lv dot lv.recognize(v) }
    def *(v: Vector): Double = { val lv = leafVector(v0); lv * lv.recognize(v) }
    def distanceSquaredTo(v: Vector): Double = { val lv = leafVector(v0); lv euclideanDistanceSquaredTo lv.recognize(v) }
    def distanceTo(v: Vector): Double = { val lv = leafVector(v0); lv euclideanDistanceTo lv.recognize(v) }

    // in place scalar mutators
    def normalize: Vector = leafVector(v0).normalize()

    def scale(scalar: Double): Vector = leafVector(v0).scale(scalar)
    def divide(divisor: Double): Vector = leafVector(v0).divide(divisor)

    def add(v: Vector): Vector = { val lv = leafVector(v0); lv.add(lv.recognize(v)) }
    def subtract(v: Vector): Vector = { val lv = leafVector(v0); lv.subtract(lv.recognize(v)) }

    // copy operations
    def copy(): Vector = leafVector(v0).copy()
    def *(d:Double): Vector = leafVector(v0) * d
    def /(d:Double): Vector = leafVector(v0) / d

    def +(v: Vector): Vector = { val lv = leafVector(v0); lv + lv.recognize(v) }
    def -(v: Vector): Vector = { val lv = leafVector(v0); lv - lv.recognize(v) }

}

