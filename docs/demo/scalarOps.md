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
val v2A = Vec[2](1.5, 2.5); v2A += 2.5; v2A
val v2B = Vec[2](1.5, 2.5); v2B -= 2.5; v2B
val v2C = Vec[2](1.5, 2.5); v2C *= 2.5; v2C
val v2D = Vec[2](1.5, 2.5); v2D /= 2.5; v2D 

v1.clampedMin(2.0)
v1.clampedMAX(2.0)

val v2 = Vec[3](1.5, 2.5, 3.5)
v2.clamped(2.0, 3.0)

// mutating (no copy) versions
val v2E = Vec[2](1.5, 2.5); v2E.min(2.0); v2E
val v2F = Vec[2](1.5, 2.5); v2F.MAX(2.0); v2F
v2.copy.clamp(2.0, 3.0)

```
