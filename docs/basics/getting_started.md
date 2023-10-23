# Getting started

## Creating a vector

Let's create some vectors

```scala mdoc
import slash.vector.*

val v = Vec.fromTuple(1.0, 2.0, 3.0, 4.0, 5.0)
val v2 = Vec[5](1.0, 2.0, 3.0, 4.0, 5.0)
val v_fill = Vec.fill[5](1.0)

val v_zeros = Vec.zeros[5]
val v_ones = Vec.ones[5]


val v_rand = Vec.random[5]()
val v_rand_max_min = Vec.random[5](2.0, 0.5)
```

And do some stuff

```scala mdoc

v + v_fill

v2.dot(v_zeros)

v_rand -= v_ones

// native indexing is fast...
v_ones(1) = 2.0
v_ones(4)
v_ones

v.mean

v.corr(v_rand)

v_rand.elementRanks

```