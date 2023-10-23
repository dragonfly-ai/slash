
# Design Choices

This library learns from and sets out to optimse the tradeoffs found in vector math libraries. The thought process for which is illustrated below, although with a summary of the implications of other design choices.

### Combining strengths

We want

- a Vector type with more robust compile time error detection than the [Case Class](#case-classes) method
- more flexibility than the [Traits and Wrappers](#wrappers-and-traits) method
- native [Array performance](#arrays) and memory characteristics

[Here's the proposal](https://github.com/dragonfly-ai/vector/blob/cb962a3b9d154eea37ffec877b25fa256e374ba7/vector/shared/src/main/scala/ai/dragonfly/math/vector/package.scala#L27)

```scala
import narr.*
import scala.compiletime.ops.int.*

package object vector {

  // What if the dimension occupied the type parameter?
  // Now we can have the syntax of case class vectors on types that reduce to pure natively typed arrays at runtime!
  opaque type Vec[N <: Int] = NArray[Double] // NArray is a type alias for the best available native Array type.

  object Vector {
    // convenient factory methods for Vec[2], Vec[3], and Vec[4]
    inline def apply(x: Double, y: Double): Vec[2] = NArray[Double](x, y)
    inline def apply(x: Double, y: Double, z: Double): Vec[3] = NArray[Double](x, y, z)
    inline def apply(x: Double, y: Double, z: Double, w: Double): Vec[4] = NArray[Double](x, y, z, w)

    // painless conversion from NArray[Double] to Vec[N]
    inline def apply[N <: Int](a: NArray[Double]): Vec[N] = { // sneaky way to cast an NArray[Double] to a Vec[N]
      dimensionCheck(a, valueOf[N])
      a
    }

    // We get convenient object oriented syntax through the use of extension methods:
    extension[N <: Int] (thisVector: Vec[N]) {
      // inline everywhere, for speed!
      inline def dimension: Int = thisVector.length
      inline def apply(index: Int): Double = thisVector(index)
      inline def update(index: Int, value: Double): Unit = thisVector(index) = value

      // immutable support (and native array copy is fast!)
      inline def +(v0: Vec[N]): Vec[N] = copy.add(v0)

      // mutable support for those who need speed more than safety:
      inline def += (v0: Vec[N]): Vec[N] = add(v0)

      // one add method for all vectors of all possible dimensions (phew maintenance)
      def add(v0: Vec[N]): Vec[N] = {
        var i = 0
        while (i < dimension) {
          thisVector(i) = thisVector(i) + v0(i)
          i = i + 1
        }
        thisVector
      }
    }
}
```
Please consider also, the flexibility to enable types that use Vectors of varying sizes:
```scala
package slash.vector
import Vector.*

// instead of putting the Vector type in the type parameter, just place the vector dimensionality.
case class VectorBounds[N <: Int](min: Vec[N], MAX: Vec[N]) {
  def contains(v: Vec[N]):Boolean = {
    var o:Boolean = true
    var i:Int = 0; while(o && i < min.dimension) {
      o = min(i) <= v(i) && v(i) <= MAX(i)
      i += 1
    }
    o
  }
}
```

The below sections set out the prior art and experiments to get to the proposal above

## Case Classes

[Example](https://scastie.scala-lang.org/ClaUW7DmQOCtT2rlAhFEtg)

Vector types very commonly appear in the form of case classes like the examples below.

The Scala.js game engine [indigo](https://github.com/PurpleKingdomGames/indigo/blob/main/indigo/indigo/src/main/scala/indigo/shared/datatypes/Vector2.scala#L5) and the earliest versions of this library adopted this approach.

It has the advantage of beiong very legible and intuitive.

Yet may lag in relative performance - designing itself out of compute intensive applications. Potentially high memory overhead for what's essentially an ordered sequence of `Double`. Extending to matricies may get messy - how should case classes represent vectors of high dimension or dimensions determined at run time?

Scala case classes don't readily serialize efficiently to JSON or Binary, weakening the cross platform story, and making them apparently less useful to native Java, C, and JavaScript developers who have attractive alternatives like native arrays.

Case class implementations tend to appear in three major flavors: Immutable, Mutable, and Hybrid:

### Strictly Immutable

```scala
case class Vector2(x:Double, y:Double) {
  def + (that:Vector2):Vector2 = Vector2(
    x + that.x, y + that.y
  )
}

case class Vector3(x:Double, y:Double, z:Double) {
  def + (that:Vector3):Vector3 = Vector3(
    x + that.x, y + that.y, z + that.z
  )
}
```

### Strictly Mutable Version

```scala
case class Vector2(var x:Double, var y:Double) {
  def += (that:Vector2):Vector2 = {
    x += that.x; y += that.y
    this
  }
}

case class Vector3(var x:Double, var y:Double, var z:Double) {
  def += (that:Vector3):Vector3 = {
    x += that.x; y += that.y; z += that.z
    this
  }
}
```

### Hybrid Version

May offer Immutable or Mutable Behavior:

```scala
case class Vector2(var x:Double, var y:Double) {
  def + (that:Vector2):Vector2 = Vector2(
    x + that.x, y + that.y
  )
  def += (that:Vector2):Vector2 = {
    x += that.x; y += that.y
    this
  }
}

case class Vector3(var x:Double, var y:Double, var z:Double) {
  def + (that:Vector3):Vector3 = Vector3(
    x + that.x, y + that.y, z + that.z
  )
  def += (that:Vector3):Vector3 = {
    x += that.x; y += that.y; z += that.z
    this
  }
}
```

#### Advantages

Intuitive syntax.

```scala
val v = Vector2(0.5, 0.25) + Vector2(0.5, 0.25)
```

Type Safe.

```scala
// compiler error:
val v3 = Vector2(0.5, 0.25) + Vector3(0.5, 0.25, 0.125)
```

Potentially Strictly Immutable

Human Readable `toString` => `Vector2(1.0,0.5)`.

- Supports Overloaded Operators: `+, -, *, /, etc.`
- No runtime errors.
- Dimensionality built into the type; no runtime dimension checking
-No loop overhead for + method

#### Notes
Not generic:  Suppose we need a `VectorBounds` class which can tell whether a vector lies within a rectangular volume or not.  Because `Vector2` and `Vector3` are entirely distinct types, we need separate implementations of `VectorBounds` for every possible dimensionality.

Tempts users into relying on `.equals` and `.hashcode`.  However, in practice, floating point errors make `Vector` vector data error prone when used as `Map` keys or in equality testing.  For example:

```scala
// Returns true only sometimes:
v2.equals( v2.rotate(Math.PI / 4.0).rotate(-Math.PI / 4.0) )
```

Not portable.  `Array[Double]` has long served as the common currency between machine learning, statistics, matrix, and other math libraries on all of Scala's target platforms.  Case classes, by contrast, require conversions to make use of 3rd party libraries.

Bloated default serializations.  Whether JSON, or binary, automatic serializations of case classes create more bloated formats than `Array[Double]`.  For example, we might prefer JSON in the format:
`[1.0, 2.0, 3.0]` instead of `{ "x" : 1.0, "y" : 2.0, "z" : 3.0 }`.

## Wrappers and Traits

[Example](https://scastie.scala-lang.org/oH0TYuYERCa8w21NeKjwrw)

Increase flexibility and may reduce maintenance costs [Prototype versions](https://github.com/dragonfly-ai/vector/blob/c9e370545d96a8e341b63e1c4ee39be846b0f970/vector/shared/src/main/scala/ai/dragonfly/math/vector/Vector3.scala#L25) of this library as recent as 2023 used this approach.  With inheritance, type tricks, and wrappers, this kind of design can consolidate most vector operations e.g. `+`, `-`, `magnitude`, and `scale` across all vector dimension types into a shared trait.

Unfortunately, the design becomes complex and error prone, without improving performance much.

```scala
def dimCheck(sup:Int, req: Int): Unit = {
  if(sup!=req) throw Exception("Mismatched Dimensions!")
}

trait Vector {
  type VEC <: Vector
  val dimension:Int

  def apply(i:Int):Double
  def update(i:Int, d:Double): Unit
  def zeros:VEC
  inline def +(that:VEC):VEC = {
    dimCheck(dimension, that.dimension)
    val out = zeros
    var i = 0; while(i < dimension) {
      out(i) = this(i) + that(i)
      i = i + 1
    }
    out
  }
}

object Vector2 {
  def apply(values:Array[Double]): Vector2 = {
    dimCheck(2, values.length); new Vector2(values)
  }
  def apply(x:Double, y:Double): Vector2 = {
    new Vector2(Array[Double](x, y))
  }
}

class Vector2 private (values:Array[Double]) extends Vector {
  type VEC = Vector2
  override val dimension:Int = 2
  inline def x:Double = values(0)
  inline def y:Double = values(1)
  inline override def apply(index: Int): Double = values(index)
  inline override def update(index: Int, value: Double): Unit = {
    values(index) = value
  }
  override def zeros:Vector2 = Vector2(0.0, 0.0)
  override def toString:String = s"Vector2($x, $y)"
}

object Vector3 {
  def apply(values:Array[Double]): Vector3 = {
    dimCheck(3, values.length); new Vector3(values)
  }
  def apply(x:Double, y:Double, z:Double): Vector3 = {
    new Vector3(Array[Double](x, y, z))
  }
}

class Vector3 private (values:Array[Double]) extends Vector {
  type VEC = Vector3
  override val dimension:Int = 3
  inline def x:Double = values(0)
  inline def y:Double = values(1)
  inline def z:Double = values(1)

  inline override def apply(index: Int): Double = values(index)
  inline override def update(index: Int, value: Double): Unit = {
    values(index) = value
  }

  override def zeros:Vector3 = Vector3(0.0, 0.0, 0.0)
  override def toString:String = s"Vector3($x, $y, $z)"
}
```

### Notes

Intuitive syntax.

```scala
val v = Vector2(0.5, 0.25) + Vector2(0.5, 0.25)
```

Type Safe.

```scala
// compiler error:
val v3 = Vector2(0.5, 0.25) + Vector3(0.5, 0.25, 0.125)
```

Potentially Strictly Immutable
Allows overrides for `toString`.
Supports Overloaded Operators: `+, -, *, /, etc.`

No runtime errors from operations involving multiple vectors of the same, explicitly defined, low dimensional type.
Generic.  Supports syntax like:

```scala
case class VectorBounds[V <: Vector](min:V, MAX:V) {
  def contains(v: V): Boolean = {
    var o: Boolean = true
    var i: Int = 0; while (o && i < min.dimension) {
      o = min.component(i) <= v.component(i) && v.component(i) <= MAX.component(i)
      i += 1
    }
    o
  }
}
```

Shares common methods between vector implementations instead of forcing unique implementations for every possible dimensionality.
Only one implementation for every possible vector dimension (!)
Accommodates higher dimensional vectors, but without compile time dimensionality-linked type safety:

```scala
class VectorN private (override val values:Array[Double]) extends Vector { }
```
Disadvantages:

- Complicated!
- Slow: relies on traits and wrappers for convenient syntax at the expense of runtime performance
- Memory intensive: suffers from too many runtime dimension checks, wrapper instantiations, and trait abstraction overhead

Allows overrides for `hashCode` and `equals` which tempts developers into making bad business logic decisions.

- Runtime dimensionality checks on every operation involving two vectors
- Exposes some runtime errors when creating Vector types from arrays of incorrect length

Slightly less bloated default serializations.  For example, we might prefer JSON in the format:
`[1.0, 2.0, 3.0]` instead of `{ "values" : [1.0, 2.0, 3.0] }`.


## Arrays

[Example](https://scastie.scala-lang.org/VVTNglXrSrW8uDrd9iYmyg)

Performance first taking advatnage of native runtime array optimisations. Minimizes memory footprint, and unless it abandons runtime dimension checking, also maximizes speed.

Unfortunately, emphasizing performance in this way may lead to runtime errors and unpleasant syntax.

```scala
type Vector = Array[Double]

object Vector {
  def plus(v1:Vector, v2:Vector):Vector = {
    if (v1.length != v2.length) {
      throw Exception("Mismatched Vector Dimensions!")
    } else {
      val out: Vector = new Array[Double](v1.length)
      var i:Int = 0
      while (i < v1.length) {
        out(i) = v1(i) + v2(i)
        i += 1
      }
      out
    }
  }
}
```

Notes:

- Fast, allowing for compute intense tasks at (near) native performance
- Minimal memory footprint
- Potentially simplest code base to maintain


Optimally concise default serialization formats with JSON defaulting to:
`[1.0, 2.0, 3.0]`

Disadvantages:
No vector dimensionality errors detected at compile time; they only appear as runtime errors.
Potentially poor user syntax:

```scala
val v1:Vector = Array[Double](1.0, 2.0)
val v2:Vector = Array[Double](3.0, 4.0, 5.0)
// runtime exception instead of compiler error
val v3:Vector = Vector.plus(v1, v2)
```

Small performance penalty from requiring dimensionality checks on every operation involving two vectors
