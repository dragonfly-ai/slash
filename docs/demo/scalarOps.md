## Vector Scalar Operations


Example

```scala mdoc
import slash.vector.*

val v1 = Vec[2](1.5, 2.5)
v1 + 2.0
v1 - 2.0
v1 * 2.0
v1 / 2.0

v1.unary_-

// mutating (no copy) methods
val v2A = Vec[2](1.5, 2.5)
v2A += 2.5
v2A.show

val v2B = Vec[2](1.5, 2.5)
v2B -= 2.5
v2B.show

val v2C = Vec[2](1.5, 2.5)
v2C *= 2.5
v2C.show

val v2D = Vec[2](1.5, 2.5)
v2D /= 2.5
v2D

v1.clampedMin(2.0).show
v1.clampedMAX(2.0).show

val v2 = Vec[3](1.5, 2.5, 3.5)
v2.clamped(2.0, 3.0)




```
