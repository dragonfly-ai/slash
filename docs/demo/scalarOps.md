## Vector Scalar Operations


Example

```scala mdoc
import ai.dragonfly.math.vector.*

val v1 = Vec[2](1.5, 2.5)
v1 + 2.0
v1 - 2.0
v1 * 2.0
v1 / 2.0

v1.unary_-

// mutating (no copy) methods
Vec[2](1.5, 2.5) += 2.5
Vec[2](1.5, 2.5) -= 2.5
Vec[2](1.5, 2.5) *= 2.5
Vec[2](1.5, 2.5) /= 2.5

//
v1.clampedMin(2.0)
v1.clampedMAX(2.0)

val v2 = Vec[3](1.5, 2.5, 3.5)
v2.clamped(2.0, 3.0)

// mutating (no copy) versions
Vec[2](1.5, 2.5).clampMin(2.0)
Vec[2](1.5, 2.5).clampMAX(2.0)
v2.copy.clamp(2.0, 3.0)

```
